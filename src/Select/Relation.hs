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

import Select.Expression

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
    (Expression (scope, variable))
  deriving (Read,Show,Eq,Functor,Foldable,Traversable)

-- | Identifier for a relation
type RelationIdentifier = Relation String String FilePath

data RelationError = BadExpressionError ExpressionError

data Table v = Table
  { columnNames :: [v]
  , rows :: [[Value]]
  }

getName (AS _ name) = name
getThing (AS x _) = x
evaluateRelation :: Relation s v Table -> Either RelationError Table
evaluateRelation (TABLE table) = Right table
evaluateRelation (FROM namedExprs rel) =
  case evaluateRelation rel of
    Right res ->
      let variableNames = columnNames res
          exprs = map getThing namedExprs
          newNames = map getName namedExprs
          getSelectedRow row = map (evaluateExpression (zip variableNames row)) exprs
      in
        Right Table { columnNames = newNames, rows = map getSelectedRow $ rows res }
    e -> e
evaluateRelation (WHERE rel predicate) =
  case evaluateRelation rel of
    Right res ->
      let variableNames = columnNames res
          evalRow row = induceBool $ evaluateExpression (zip variableNames row) predicate
          processRow e@(Left _) _ = e
          processRow (Right selected) row =
            case evalRow row of
              BoolValue b -> Right $ if b then selected ++ [row] else selected
              ErrorValue e -> Left e
              v -> Left $ TypeError v
          selectedRows = foldl processRow (Right []) $ rows res
          rowsToRes theRows = res { rows = theRows }
      in
        rowsToRes <$> selectedRows
    e -> e
evaluateRelation (INNER_JOIN_ON rel1 rel2 predicate) =
  case (evaluateRelation $ getThing rel1, evaluateRelation $ getThing rel2) of
    (Right t1, Right t2) ->
      let t1Name = getName rel1
          t2Name = getName rel2
          t1VarNames = (t1Name,) <$> columnNames t1
          t2VarNames = (t2Name,) <$> columnNames t2
          bindings r1 r2 = zip t1VarNames r1 ++ zip t2VarNames r2
          includePair r1 r2 = induceBool $ evaluateExpression (bindings r1 r2) predicate
          cartesianProduct = [(r1, r2) | r1 <- rows t1, r2 <- rows t2]
          processRow e@(Left _) _ = e
          processRow (Right selected) (r1, r2) =
            case includePair r1 r2 of
              BoolValue b -> Right $ if b then selected ++ [r1 ++ r2] else selected
              ErrorValue e -> Left e
              v -> Left $ TypeError v
          selectedRows = foldl processRow (Right []) cartesianProduct
          newColumns = map (printf "%s.%s" t1Name) (columnNames t1) ++
                       map (printf "%s.%s" t2Name) (columnNames t2)
          makeTableFromRows sel = Table { rows = sel, columnNames = newColumns }
      in
        makeTableFromRows <$> selectedRows
    (Left e, _) -> Left e
    (_, Left e) -> Left e
evaluateRelation (UNION rel1 rel2) =
  case (evaluateRelation rel1, evaluateRelation rel2) of
    (Right t1, Right t2) ->
      let cn1 = columnNames t1
          cn2 = columnNames t2
          newTableColumns = nub $ cn1 ++ cn2
          makeNewRows table =
            let namesLookup = makeIndexedNames $ columnNames table
                getValue row columnName =
                    maybe (StringValue "") (row !!) $ lookup columnName namesLookup
                makeRow row = map (getValue row) newTableColumns
            in
              map makeRow $ rows table
      in
        Right Table { columnNames = newTableColumns
                    , rows = makeNewRows t1 ++ makeNewRows t2
                    }
    (Left v, _) -> Left v
    (_, Left v) -> Left v
  where
    makeIndexedNames cn =
          let indexes = [0..(length cn - 1)] in zip cn indexes
