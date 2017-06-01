{-|
Module      : Table
Description : Internal representation of a table
Copyright   : (c) Ivan Malison
Maintainer  : IvanMalison@gmail.com
Stability   : experimental
Portability : POSIX

-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE RankNTypes #-}

module Table
  ( evaluateRelation
  , Table(..)
  , TableError(..)
  , TableValue(..)
  ) where

import Data.Maybe
import Select.Expression
import Select.Relation
import Text.Printf
import Text.Read

data TableError
  = ColumnDoesNotExistError String
  | ReadColumnError String String
  | TypeError TableValue
  | UnaryExpressionError String TableValue
  | BinaryExpressionError String TableValue TableValue
    deriving (Read, Show)

data TableValue
  = AnyValue String
  | StringValue String
  | BoolValue Bool
  | IntValue Int
  | RealValue Double
  | ErrorValue TableError
    deriving (Read, Show)

data Table = Table
  { columnNames :: [String]
  , rows :: [[TableValue]]
  }

induceBool :: TableValue -> TableValue
induceBool b@(BoolValue _) = b
induceBool (AnyValue str) =
  maybe (ErrorValue $ ReadColumnError "Bool" str) BoolValue $ readMaybe str
induceBool v = ErrorValue $ TypeError v

boolAct :: TableValue -> (Bool -> TableValue) -> TableValue
boolAct v fn =
  case induceBool v of
    BoolValue b -> fn b
    e -> e

boolActExpr
  :: (Show a, Eq a)
     => [(a, TableValue)]
     -> Expression a -> (Bool -> Bool) -> TableValue
boolActExpr namedRow expr fn =
  boolAct (evaluateExpression namedRow expr) $ BoolValue . fn

induceNumeric :: TableValue -> TableValue
induceNumeric i@(IntValue _) = i
induceNumeric r@(RealValue _) = r
induceNumeric (AnyValue str) =
  maybe (ErrorValue $ ReadColumnError "Real" str) RealValue $ readMaybe str
induceNumeric v = ErrorValue $ TypeError v

induceIntFirst :: TableValue -> TableValue
induceIntFirst v@(AnyValue str) =
  maybe (induceNumeric v) IntValue $ readMaybe str
induceIntFirst v = v

induceString s@(StringValue _) = s
induceString (AnyValue str) = StringValue str
induceString v = ErrorValue $ TypeError v

binaryComparison
  :: (Show a, Eq a)
  => [(a, TableValue)]
  -> (forall c. Ord c =>
                  (c -> c -> Bool))
  -> Expression a
  -> Expression a
  -> TableValue
binaryComparison namedRow operator ex1 ex2 =
  case evaluateExpression namedRow ex1 of
    StringValue s1 ->
      case induceString $ evaluateExpression namedRow ex2 of
        StringValue s2 -> BoolValue $ operator s1 s2
        e -> e
    BoolValue b1 ->
      case induceBool $ evaluateExpression namedRow ex2 of
        BoolValue b2 -> BoolValue $ operator b1 b2
        e -> e
    RealValue r1 -> case induceNumeric $ evaluateExpression namedRow ex2 of
      IntValue i2 -> BoolValue $ operator r1 $ fromIntegral i2
      RealValue r2 -> BoolValue $ operator r1 r2
      e -> e
    IntValue i1 ->
      case induceIntFirst $ evaluateExpression namedRow ex2 of
        IntValue i2 -> BoolValue $ operator i1 i2
        RealValue r2 -> BoolValue $ operator (fromIntegral i1) r2
        e -> e
    ErrorValue e -> ErrorValue e
    AnyValue s1 -> let value = AnyValue s1 in
      case evaluateExpression namedRow ex2 of
        BoolValue b2 ->
          case induceBool value of
            BoolValue b1 -> BoolValue $ operator b1 b2
            e -> e
        StringValue s2 -> BoolValue $ operator s1 s2
        RealValue r2 ->
          case induceNumeric value of
            IntValue i1 -> BoolValue $ operator (fromIntegral i1) r2
            RealValue r1 -> BoolValue $ operator r1 r2
            e -> e
        IntValue i2 ->
          case induceIntFirst value of
            IntValue i1 -> BoolValue $ operator i1 i2
            RealValue r1 -> BoolValue $ operator r1 (fromIntegral i2)
            e -> e
        ErrorValue e -> ErrorValue e
        AnyValue s2 -> BoolValue $ operator s1 s2

numericOperation
  :: (Show a, Eq a)
  => [(a, TableValue)]
  -> (forall c. Num c =>
                  (c -> c -> c))
  -> Expression a
  -> Expression a
  -> TableValue
numericOperation namedRow operator ex1 ex2 =
  case induceIntFirst $ evaluateExpression namedRow ex1 of
    IntValue i1 ->
      case induceIntFirst $ evaluateExpression namedRow ex2 of
        IntValue i2 -> IntValue $ operator i1 i2
        RealValue r2 -> RealValue $ operator (fromIntegral i1) r2
        e -> e
    RealValue r1 ->
      case induceNumeric $ evaluateExpression namedRow ex2 of
        IntValue i2 -> RealValue $ operator r1 (fromIntegral i2)
        RealValue r2 -> RealValue $ operator r1 r2
        e -> e
    e -> e

evaluateExpression :: (Show a, Eq a) => [(a, TableValue)] -> Expression a -> TableValue
evaluateExpression namedRow expr =
  case expr of
    LiteralBool b -> BoolValue b
    LiteralInt i -> IntValue i
    LiteralReal d -> RealValue d
    LiteralString s -> StringValue s
    Column v ->
      fromMaybe (ErrorValue $ ColumnDoesNotExistError $ show v) $ lookup v namedRow
    Not negated -> boolActExpr namedRow negated not
    Neg negated ->
      case induceIntFirst $ evaluateExpression namedRow negated of
        IntValue i -> IntValue (-i)
        RealValue r -> RealValue (-r)
        e -> e
    And ex1 ex2 ->
      case ( induceBool $ evaluateExpression namedRow ex1
           , induceBool $ evaluateExpression namedRow ex2) of
        (BoolValue a, BoolValue b) -> BoolValue $ a && b
        (v1, v2) -> ErrorValue $ BinaryExpressionError (show expr) v1 v2
    Or ex1 ex2 ->
      case ( induceBool $ evaluateExpression namedRow ex1
           , induceBool $ evaluateExpression namedRow ex2) of
        (BoolValue a, BoolValue b) -> BoolValue $ a || b
        (v1, v2) -> ErrorValue $ BinaryExpressionError (show expr) v1 v2
    Equ ex1 ex2 -> binaryComparison namedRow (==) ex1 ex2
    Neq ex1 ex2 -> binaryComparison namedRow (/=) ex1 ex2
    Gt  ex1 ex2 -> binaryComparison namedRow (>)  ex1 ex2
    Gte ex1 ex2 -> binaryComparison namedRow (>=) ex1 ex2
    Lt  ex1 ex2 -> binaryComparison namedRow (<)  ex1 ex2
    Lte ex1 ex2 -> binaryComparison namedRow (<=) ex1 ex2
    Add ex1 ex2 -> numericOperation namedRow (+)  ex1 ex2
    Sub ex1 ex2 -> numericOperation namedRow (-)  ex1 ex2
    Mul ex1 ex2 -> numericOperation namedRow (*)  ex1 ex2
    Div ex1 ex2 ->
      case induceIntFirst $ evaluateExpression namedRow ex1 of
        IntValue i1 ->
          case induceIntFirst $ evaluateExpression namedRow ex2 of
            IntValue i2 -> IntValue $ i1 `div` i2
            RealValue r2 -> RealValue $ fromIntegral i1 / r2
            e -> e
        RealValue r1 ->
          case induceNumeric $ evaluateExpression namedRow ex2 of
            IntValue i2 -> RealValue $ r1 / fromIntegral i2
            RealValue r2 -> RealValue $ r1 / r2
            e -> e
        e -> e
    Mod ex1 ex2 ->
      case induceIntFirst $ evaluateExpression namedRow ex1 of
        IntValue i1 ->
          case induceIntFirst $ evaluateExpression namedRow ex2 of
            IntValue i2 -> IntValue $ i1 `mod` i2
            RealValue r2 -> ErrorValue $ TypeError $ RealValue r2
            e -> e
        RealValue r1 -> ErrorValue $ TypeError $ RealValue r1
        e -> e

getName (AS _ name) = name
getThing (AS x _) = x

evaluateRelation :: Relation String String Table -> Either TableError Table
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
              BoolValue b -> Right $ if b then row:selected else selected
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
              BoolValue b -> Right $ if b then (r1 ++ r2):selected else selected
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
