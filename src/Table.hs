{-|
Module      : Table
Description : Internal representation of a table
Copyright   : (c) Ivan Malison
Maintainer  : IvanMalison@gmail.com
Stability   : experimental
Portability : POSIX

-}

module Table where

import Select.Relation
import Data.Maybe
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

induceBool b@(BoolValue _) = b
induceBool (AnyValue str) =
  maybe (ErrorValue $ ReadColumnError "Bool" str) BoolValue $ readMaybe str
induceBool v expr = ErrorValue $ TypeError v

boolAct v fn =
  case induceBool $ v of
    BoolValue b -> fn b
    e -> e

boolActExpr namedRow expr fn =
  boolAct (evaluateExpression namedRow expr) $ BoolValue . fn

induceNumeric i@(IntValue _) = i
induceNumeric r@(RealValue _) = r
induceNumeric (AnyValue str) =
  maybe (ErrorValue $ ReadColumnError "Real" str) RealValue $ readMaybe str
induceNumeric v expr = ErrorValue $ TypeError v

induceIntFirst v@(AnyValue str) =
  maybe (induceNumeric v) IntValue $ readMaybe str

induceString s@(StringValue _) = s
induceString (AnyValue str) = StringValue str
induceString v = ErrorValue $ TypeError v

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
        RealValue r2 -> BoolValue $ operator (fromIntegral i1) i2
        e -> e
    ErrorValue e -> ErrorValue e
    AnyValue s1 -> let value = AnyValue str in
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
            RealValue r1 -> BoolValue $ operator i1 (fromIntegral i2)
            e -> e
        ErrorValue e -> ErrorValue e
        AnyValue s2 -> BoolValue $ operator s1 s2

numericOperation namedRow operator ex1 ex2 =
  case induceIntFirst $ evaluateExpression namedRow ex1 of
    IntValue i1 ->
      case induceIntFirst $ evaluateExpression namedRow ex2 of
        IntValue i2 -> IntValue $ operator i1 i2
        RealValue r2 -> RealValue $ operator (fromIntegral i1) r2
        e -> e
    RealValue r1 ->
      case induceNumeric $ evaluateExpression namedRow ex2 of
        IntValue i2 -> RealValue $ operator i1 (fromIntegral i2)
        RealValue r2 -> RealValue $ operator r1 r2
        e -> e
    e -> e

evaluateExpression namedRow expr =
  case expr of
    LiteralBool b -> BoolValue b
    LiteralInt i -> IntValue i
    LiteralReal d -> RealValue d
    LiteralString s -> StringValue s
    Column v ->
      maybe (ErrorValue $ ColumnDoesNotExistError $ show v) AnyValue $
      lookup v namedRow
    Not negated -> boolActExpr namedRow negated not
    Neg negated ->
      case induceIntFirst $ evaluateExpression negated of
        IntValue i -> IntValue -i
        RealValue r -> RealValue -r
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
    Div ex1 ex2 -> numericOperation namedRow (/)  ex1 ex2
    Mod ex1 ex2 -> numericOperation namedRow mod  ex1 ex2

evaluateRelation :: Relation String String Table -> Either TableError Table
evaluateRelation TABLE table = table
evaluateRelation FROM namedExprs rel =
  let res = evaluateRelation rel
      variableNames = columnNames res
      exprs = map (AS expr _ -> expr) namedExprs
      newNames = map (AS _ name -> name) namedExprs
      getSelectedRow row =
        map (evaluateExpression (zip variableNames row)) exprs
  in
    Right $ Table { columnNames = newNames, rows = map getSelectedRow $ rows res }
evaluateRelation WHERE rel pred =
  let res = evaluateRelation rel
      variableNames = columnNames res
      evalRow row =
        case induceBool $ evaluateExpression zip variableNames columnNames of
          BoolValue b -> b
          e -> e
      in
        res { rows = filter evaluateExpression zip }
