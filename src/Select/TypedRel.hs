{-|
Module      : Shroudbase.Protocol.Ast.Relation
Description : Parse SELECT queries
Copyright   : (c) LeapYear Technologies 2016
Maintainer  : eitan@leapyear.io
Stability   : experimental
Portability : POSIX

Relations
-}

{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TupleSections #-}

-- TODO: Add export list
module Select.Relation where

import           Control.Monad
import           Data.Either
import           Data.List
import           Data.List.Split
import           Data.Maybe
import qualified Pipes.Prelude as Plude
import qualified Data.Set as S
import           Data.Typeable
import           Debug.Trace
import           Pipes
import           Text.Printf

import           Select.Expression

infix 1 `FROM`
infixr 2 `UNION`
infix 3 `INNER_JOIN_ON`
infix 4 `WHERE`

-- | Identifier for a relation
type RelationIdentifier = Relation String String FilePath

-- | Relation abstract syntax tree
data Relation scope variable table
  = TABLE table
  | FROM
    [Expression variable `As` scope] -- projection
    (Relation scope variable table)
  | WHERE
    (Relation scope variable table)
    (TypedExp Bool variable) -- predicate
  | UNION (Relation scope variable table) (Relation scope variable table)
  | INNER_JOIN_ON
    (Relation scope variable table `As` scope)
    (Relation scope variable table `As` scope)
    (TypedExp Bool (scope, variable))

convertRelation :: (t -> IO u) -> Relation s v t -> IO (Relation s v u)
convertRelation convert rel =
  case rel of
    TABLE t -> TABLE <$> convert t
    FROM exps rel -> FROM exps <$> recurse rel
    WHERE rel exp -> WHERE <$> recurse rel <*> pure exp
    UNION rel1 rel2 -> UNION <$> recurse rel1 <*> recurse rel2
    INNER_JOIN_ON (AS rel1 name1) (AS rel2 name2) exp ->
      INNER_JOIN_ON <$> (flip AS name1 <$> recurse rel1)
                    <*> (flip AS name2 <$> recurse rel2)
                    <*> pure exp
  where recurse = convertRelation convert

data RelationError
  = BadExpressionError
  | ConflictingTypeError
  | UnknownColumnError
  | UnmatchedUnion
  | BadValueError String
    deriving Show

type TypeRequirement = STypeRep
type TypeRequirements v = [(v, TypeRequirement)]

getReqsFromExprs
  :: Eq v
  => [Expression v] -> Either RelationError (TypeRequirements v)
getReqsFromExprs exprs =
  mapM getTypeRequirements exprs >>= mergeTypeRequirements

getTypeRequirements :: Eq v => Expression v -> Either RelationError (TypeRequirements v)
getTypeRequirements (Expression te) =
  case te of
    Literal v -> Right []
    c@(Column name) -> Right [(name, typeOfTypedExp c)]
    And e1 e2 -> getReqs e1 e2
    Gt  e1 e2 -> getReqs e1 e2
    Gte e1 e2 -> getReqs e1 e2
    Lt  e1 e2 -> getReqs e1 e2
    Lte e1 e2 -> getReqs e1 e2
    Or  e1 e2 -> getReqs e1 e2
    Equ e1 e2 -> getReqs e1 e2
    Neq e1 e2 -> getReqs e1 e2
    Add e1 e2 -> getReqs e1 e2
    Sub e1 e2 -> getReqs e1 e2
    Mul e1 e2 -> getReqs e1 e2
    Mod e1 e2 -> getReqs e1 e2
    Not e1 -> getTypeRequirements $ Expression e1
    Neg e1 -> getTypeRequirements $ Expression e1
  where
    getReqs
      :: (Eq v, STypeable t)
      => TypedExp t v
      -> TypedExp t v
      -> Either RelationError (TypeRequirements v)
    getReqs e1 e2 = getReqsFromExprs [Expression e1, Expression e2]

mergeTypeRequirements
  :: Eq v
  => [TypeRequirements v] -> Either RelationError (TypeRequirements v)
mergeTypeRequirements requirements =
  foldl handleName (Right []) allNames
  where allNames = nub $ requirements >>= (map fst)
        handleName theEither columnName = theEither >>= \newReqs ->
          let allTypes = catMaybes $ map (lookup columnName) requirements
              allMatch = all (== head allTypes) (tail allTypes)
          in
            if allMatch
            then Right $ (columnName, head allTypes):newReqs
            else Left $ ConflictingTypeError

checkRequirements reqs exps =
  if all (flip elem columnTypes) reqs
  then Right columnTypes
  else Left ConflictingTypeError -- TODO: this could actually be a bunch of stuff
  where columnTypes = map expToReq exps
        expToReq (AS (Expression te) name) = (name, typeOfTypedExp te)

data TypedRowProducer m v
  = TypedRowProducer { columnTypes :: TypeRequirements v
                     , rowProducer :: Producer [Value] m ()
                     }

nameRow typedProducer =
  let producerTypes = columnTypes typedProducer
      producerNames = map fst producerTypes
      nameRow row = zip producerNames row
  in nameRow

class (Eq v, Monad m) => BuildsRowProducer t m v where
  getRowProducer
    :: t
    -> [(v, TypeRequirement)]
    -> (Either RelationError (TypedRowProducer m v))

class NameCombine n where
  -- Law is combine . split = id when okayToSplit n
  combineName :: (n, n) -> n
  splitName :: n -> (n, n)
  okayToSplit :: n -> Bool

instance NameCombine String where
  combineName (a, b) = printf "%s.%s" a b
  splitName name =
    let nameList = splitOn "." name
    in (head nameList, head $ tail nameList)
  okayToSplit n = length (filter (== '.') n) == 1

splitRequirements reqs =
  map helper reqs
  where helper (name, t) = (splitName name, t)

groupReqs reqs =
  map unscopeGroup $ groupBy shouldGroup reqs
  where shouldGroup ((s1, _), _) ((s2, _), _) = s1 == s2
        unscope req = (snd $ fst req, snd req)
        unscopeGroup agroup = (fst $ fst $ head agroup, map unscope agroup)

relationToRowProducer
  :: (BuildsRowProducer t m v, Eq v, Show v, NameCombine v)
  => TypeRequirements v
  -> Relation v v t
  -> Either RelationError (TypedRowProducer m v)
relationToRowProducer reqs rel =
  case rel of

    TABLE t -> getRowProducer t reqs

    FROM exps rel ->
      do
        newColumnTypes <- checkRequirements reqs exps
        let unnamed = map (\(AS exp _) -> exp) exps
        newReqs <- getReqsFromExprs unnamed
        typedProducer <- relationToRowProducer newReqs rel
        let rowNamer = nameRow typedProducer
            producerRows = rowProducer typedProducer
            handleRow row =
              let namedRow = rowNamer row
                  eval (Expression te) =
                    case Value <$> evaluateExpression namedRow te of
                      Left _ -> undefined -- XXX: this should never happen
                      Right v -> v
              in yield $ map eval unnamed
            thisProducer = for producerRows handleRow
        return TypedRowProducer
                 { columnTypes = newColumnTypes
                 , rowProducer = thisProducer
                 }

    WHERE rel pred ->
      do
        predReqs <- getTypeRequirements (Expression pred)
        merged <- mergeTypeRequirements [predReqs, reqs]
        typedProducer <- relationToRowProducer merged rel
        let rowNamer = nameRow typedProducer
            handleRow row =
              case evaluateExpression (rowNamer row) pred of
                Right b -> if b then yield row else discard row
                Left _ -> undefined -- XXX: hmm what to do here
            thisProducer = for (rowProducer typedProducer) handleRow
        return typedProducer { rowProducer = thisProducer }

    INNER_JOIN_ON (AS rel1 scope1) (AS rel2 scope2) pred ->
      do
        predReqs <- getTypeRequirements (Expression pred)
        -- TODO: make sure splits are okay
        let reqPairs = splitRequirements reqs ++ predReqs
            grouped = groupReqs reqPairs
            rel1Reqs = fromMaybe [] $ lookup scope1 grouped
            rel2Reqs = fromMaybe [] $ lookup scope2 grouped
        producer1 <- relationToRowProducer rel1Reqs rel1
        producer2 <- relationToRowProducer rel2Reqs rel2
        let nameRowScoped producer scope =
              let producerTypes = columnTypes producer
                  producerNames = map fst producerTypes
                  scopedNames = map (scope,) producerNames
                  nameRow row = zip scopedNames row
              in nameRow
            nameRow1 = nameRowScoped producer1 scope1
            nameRow2 = nameRowScoped producer2 scope2
            thisProducer = do
              -- All of the second table has to be loaded in to memory in this
              -- case.
              rows <- lift $ Plude.toListM (rowProducer producer2)
              let handleRowPair row1 = do
                    let innerBody row2 =
                          let combined = nameRow1 row1 ++ nameRow2 row2
                          in case evaluateExpression combined pred of
                               Right b -> if b then yield $ row1 ++ row2 else discard () -- XXX: ...
                    mapM_ innerBody $ rows
              for (rowProducer producer1) handleRowPair
            makeScopedTypes namedTypes scope =
              let addScope (n, t) = (combineName (scope, n), t) in
              map addScope namedTypes
            newColumnTypes = makeScopedTypes (columnTypes producer1) scope1 ++
                             makeScopedTypes (columnTypes producer2) scope2
        return TypedRowProducer { rowProducer = thisProducer
                                , columnTypes = newColumnTypes
                                }

    UNION rel1 rel2 -> do
      producer1 <- relationToRowProducer reqs rel1
      producer2 <- relationToRowProducer reqs rel2
      let columns1 = columnTypes producer1
          columns2 = columnTypes producer2
          permutation = catMaybes $ map (flip elemIndex columns2) columns1
          equalAsSets = all (flip elem columns1) columns2 &&
                        -- XXX: No sorting or hashing so this is the only way...
                        -- this should be small anyway
                        length columns1 == length columns2
          permuteRow row = yield $ map (row !!) permutation
      if equalAsSets then
        return producer1 { rowProducer = rowProducer producer1 >>
                                         for (rowProducer producer2) permuteRow
                         }
      else
        Left UnmatchedUnion
