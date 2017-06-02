{-|
Module      : Select.Expression
Description : Data types used for ALTER queries
Copyright   : Ivan Malison
Maintainer  : IvanMalison@gmail.com
Stability   : experimental
Portability : POSIX

Data types used for SQL expressions
-}

{-# LANGUAGE DeriveFoldable #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}


module Select.Expression
  ( As
  , TypedExp(..)
  , Expression(..)
  , ExpressionError(..)
  , Identifiable(..)
  , Named(..)
  , Value(..)
  , evaluateExpression
  , typeOfUnTypedExp
  ) where

import Data.Maybe
import Data.Unique
import Unsafe.Coerce
import System.IO.Unsafe

data IdentifiableUnique t = IdentifiableUnique Unique deriving (Eq)
getUnique :: IdentifiableUnique t -> Unique
getUnique (IdentifiableUnique u) = u

class Identifiable t where
  getTypeIdentifier :: IdentifiableUnique t

typeOfTypedExp :: Identifiable t => TypedExp t v -> IdentifiableUnique t
typeOfTypedExp _ = getTypeIdentifier

typeOfValue :: Identifiable t => t -> IdentifiableUnique t
typeOfValue _ = getTypeIdentifier

typeOfUnTypedExp :: Expression v -> Unique
typeOfUnTypedExp (Expression typed) = getUnique $ typeOfTypedExp typed

evaluate :: Identifiable t => TypedExp t v -> Value -> Either ExpressionError t
evaluate toReplace (Value with) =
  if getUnique (typeOfTypedExp toReplace) == getUnique (typeOfValue with)
  then Right $ unsafeCoerce with
  else Left TypeError

booleanUnique :: IdentifiableUnique Bool
booleanUnique = IdentifiableUnique $ unsafePerformIO newUnique
instance Identifiable Bool where
  getTypeIdentifier = booleanUnique

data Expression v = forall t. Identifiable t => Expression (TypedExp t v)
data Value = forall t. Identifiable t => Value t

data ExpressionError = TypeError | BindingError

data TypedExp t v where
  Literal :: Identifiable t => t -> TypedExp t v
  Column :: Identifiable t => v -> TypedExp t v
  Not :: TypedExp Bool v -> TypedExp Bool v
  And :: TypedExp Bool v -> TypedExp Bool v -> TypedExp Bool v
  Or  :: TypedExp Bool v -> TypedExp Bool v -> TypedExp Bool v
  Equ :: (Identifiable t, Eq t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Neq :: (Identifiable t, Eq t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Gt  :: (Identifiable t, Ord t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Gte :: (Identifiable t, Ord t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Lt  :: (Identifiable t, Ord t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Lte :: (Identifiable t, Ord t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Neg :: (Identifiable t, Num t) => TypedExp t v -> TypedExp t v
  Add :: (Identifiable t, Num t) => TypedExp t v -> TypedExp t v -> TypedExp t v
  Sub :: (Identifiable t, Num t) => TypedExp t v -> TypedExp t v -> TypedExp t v
  Mul :: (Identifiable t, Num t) => TypedExp t v -> TypedExp t v -> TypedExp t v
  Div :: (Identifiable t, Fractional t) => TypedExp t v -> TypedExp t v -> TypedExp t v
  Mod :: (Identifiable t, Integral t) => TypedExp t v -> TypedExp t v -> TypedExp t v

evaluateExpression
  :: forall v t. (Identifiable t, Eq v, Show v)
  => [(v, Value)] -> TypedExp t v -> Either ExpressionError t
evaluateExpression bindings expr =
  case expr of
    Literal v -> Right v
    c@(Column name) ->
      let mValue = lookup name bindings
          evaluated = evaluate c <$> mValue
      in fromMaybe (Left BindingError) evaluated
    Not e1 -> not <$> eval e1
    Neg e1 -> negate <$> eval e1
    And e1 e2 -> applyBinary (&&) e1 e2
    Gt  e1 e2 -> applyBinary (>)  e1 e2
    Gte e1 e2 -> applyBinary (>=) e1 e2
    Lt  e1 e2 -> applyBinary (<)  e1 e2
    Lte e1 e2 -> applyBinary (<=) e1 e2
    Or  e1 e2 -> applyBinary (||) e1 e2
    Equ e1 e2 -> applyBinary (==) e1 e2
    Neq e1 e2 -> applyBinary (/=) e1 e2
    Add e1 e2 -> applyBinary (+)  e1 e2
    Sub e1 e2 -> applyBinary (-)  e1 e2
    Mul e1 e2 -> applyBinary (*)  e1 e2
    Mod e1 e2 -> applyBinary mod  e1 e2
    Div e1 e2 -> applyBinary (/)  e1 e2
  where
    applyBinary
      :: (Identifiable t2)
      => (t2 -> t2 -> t3)
      -> TypedExp t2 v
      -> TypedExp t2 v
      -> Either ExpressionError t3
    applyBinary op e1 e2 = op <$> eval e1 <*> eval e2
    eval
      :: (Identifiable t1)
      => TypedExp t1 v -> Either ExpressionError t1
    eval = evaluateExpression bindings

-- | `Named` used for naming objects and bringing them into scope
data Named scope x = AS x scope
  deriving
    ( Read
    , Show
    , Eq
    , Functor
    , Foldable
    , Traversable
    )

-- | `As` is `Named` with type variables flipped
type As x scope = Named scope x
