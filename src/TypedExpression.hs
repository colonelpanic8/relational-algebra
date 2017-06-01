{-|
Module      : TypedExpression
Description : An alternative expression type
Copyright   : Ivan Malison
Maintainer  : IvanMalison@gmail.com
Stability   : experimental
Portability : POSIX

Data types used for SQL expressions
-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}

module TypedExpression () where

data TypedExpression t v where
  LiteralBool :: Bool -> TypedExpression Bool v
  LiteralInt :: Int -> TypedExpression Int v
  LiteralReal :: Double -> TypedExpression Double v
  LiteralString :: String -> TypedExpression String v
  Column :: v -> TypedExpression t v
  Not :: TypedExpression Bool v -> TypedExpression Bool v
  And :: TypedExpression Bool v -> TypedExpression Bool v -> TypedExpression Bool v
  Or  :: TypedExpression Bool v -> TypedExpression Bool v -> TypedExpression Bool v

  Equ :: TypedExpression t v -> TypedExpression t v -> TypedExpression Bool v
  Neq :: TypedExpression t v -> TypedExpression t v -> TypedExpression Bool v

  Gt  :: TypedExpression t v -> TypedExpression t v -> TypedExpression Bool v
  Gte :: TypedExpression t v -> TypedExpression t v -> TypedExpression Bool v
  Lt  :: TypedExpression t v -> TypedExpression t v -> TypedExpression Bool v
  Lte :: TypedExpression t v -> TypedExpression t v -> TypedExpression Bool v

  Neg :: TypedExpression t v -> TypedExpression t v
  Add :: TypedExpression t v -> TypedExpression t v -> TypedExpression t v
  Sub :: TypedExpression t v -> TypedExpression t v -> TypedExpression t v
  Mul :: TypedExpression t v -> TypedExpression t v -> TypedExpression t v
  Div :: TypedExpression t v -> TypedExpression t v -> TypedExpression t v
  Mod :: TypedExpression t v -> TypedExpression t v -> TypedExpression t v
