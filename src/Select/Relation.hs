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

module Select.Relation
  ( Relation(..)
  , RelationIdentifier
  ) where

import           Select.Expression
import qualified Select.TypedExp as TE
import qualified Select.TypedRel as TR

infix 1 `FROM`
infixr 2 `UNION`
infix 3 `INNER_JOIN_ON`
infix 4 `WHERE`

-- | Relation abstract syntax tree
data Relation scope variable table
  = TABLE table
  | FROM
    [Expression variable `As` scope] -- projection
    (Relation scope variable table)
  | WHERE
    (Relation scope variable table)
    (Expression variable) -- predicate
  | UNION (Relation scope variable table) (Relation scope variable table)
  | INNER_JOIN_ON
    (Relation scope variable table `As` scope)
    (Relation scope variable table `As` scope)
    (Expression (scope, variable)) -- predicate
  deriving (Read,Show,Eq,Functor,Foldable,Traversable)

-- | Identifier for a relation
type RelationIdentifier = Relation String String FilePath

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

data TypeRequirement
  = BoolType
  | StringType
  | IntType
  | RealType
    deriving (Eq, Show, Read, Typeable)

data TypingError = TypingError
type TypeRequirements v = [(v, [TypeRequirement])]
data InferenceTable t = InferenceTable t TypeRequirements

getTypeRequirements :: Eq v => Expression v -> Either TR.RelationError (TypeRequirements v)
getTypeRequirements exp =
  case exp of
    Literal v -> Right []
    c@(Column name) -> Right [(name, typeOfExpression c)]
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
    Not e1 -> getTypeRequirements $ AnyExpression e1
    Neg e1 -> getTypeRequirements $ AnyExpression e1
  where
    getReqs
      :: (Eq v, STypeable t)
      => Expression t v
      -> Expression t v
      -> Either RelationError (TypeRequirements v)
    getReqs e1 e2 = getReqsFromExprs [AnyExpression e1, AnyExpression e2]


-- applyRequirements
--   :: (Eq v, TR.NameCombine v)
--   => TypeRequirements v
--   -> Relation v v (InferenceTable t)
--   -> Relation v v (InferenceTable t)
-- applyRequirements reqs rel =

--   TABLE (InferenceTable t reqs) -> reqs

--   FROM exps rel -> undefined


-- toTypedRelation
--   :: (Eq v, TR.NameCombine v)
--   => TypeRequirements v
--   -> Relation v v (InferenceTable t)
--   -> Either TR.RelationError (TR.Relation v v t, TypeRequirements v)
-- toTypedRelation reqs rel =
--   case rel of

--     TABLE (InferenceTable t reqs) -> Right $ (TR.TABLE t, reqs)

--     FROM exps rel ->
--       do
--         let unnamed = map (\(AS exp _) -> exp) exps
