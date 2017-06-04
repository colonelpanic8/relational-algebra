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
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Select.Expression
  ( As
  , Expression(..)
  , ExpressionError(..)
  , Typeable(..)
  , Named(..)
  , TypedExp(..)
  , as
  , bColumn
  , colBool
  , colInt
  , colReal
  , colString
  , evaluateExpression
  , iColumn
  , rColumn
  , sColumn
  , typeOfTypedExp
  ) where

import Data.Dynamic
import Data.Maybe
import Data.Proxy
import Data.Typeable
import Data.Unique
import System.IO.Unsafe
import Text.Read
import Unsafe.Coerce

data Expression v = forall t. Typeable t => Expression (TypedExp t v)

typeOfTypedExp :: forall t v. Typeable t => TypedExp t v -> TypeRep
typeOfTypedExp _ = typeRep (Proxy :: Proxy t)

substitute
  :: Typeable t
  => TypedExp t v -> Dynamic -> Either ExpressionError t
substitute _ dyn =
  maybe (Left TypeError) Right $ fromDynamic dyn

data ExpressionError = TypeError | BindingError deriving Show

data TypedExp t v where
  Literal :: Typeable t => t -> TypedExp t v
  Column :: Typeable t => v -> TypedExp t v
  Not :: TypedExp Bool v -> TypedExp Bool v
  And :: TypedExp Bool v -> TypedExp Bool v -> TypedExp Bool v
  Or  :: TypedExp Bool v -> TypedExp Bool v -> TypedExp Bool v
  Equ :: (Typeable t, Eq t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Neq :: (Typeable t, Eq t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Gt  :: (Typeable t, Ord t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Gte :: (Typeable t, Ord t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Lt  :: (Typeable t, Ord t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Lte :: (Typeable t, Ord t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Neg :: (Typeable t, Num t) => TypedExp t v -> TypedExp t v
  Add :: (Typeable t, Num t) => TypedExp t v -> TypedExp t v -> TypedExp t v
  Sub :: (Typeable t, Num t) => TypedExp t v -> TypedExp t v -> TypedExp t v
  Mul :: (Typeable t, Num t) => TypedExp t v -> TypedExp t v -> TypedExp t v
  Div :: (Typeable t, Fractional t) => TypedExp t v -> TypedExp t v -> TypedExp t v
  Mod :: (Typeable t, Integral t) => TypedExp t v -> TypedExp t v -> TypedExp t v

evaluateExpression
  :: forall v t. (Typeable t, Eq v, Show v)
  => [(v, Dynamic)] -> TypedExp t v -> Either ExpressionError t
evaluateExpression bindings expr =
  case expr of
    Literal v -> Right v
    c@(Column name) ->
      let mValue = lookup name bindings
          evaluated = substitute c <$> mValue
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
      :: (Typeable t2)
      => (t2 -> t2 -> t3)
      -> TypedExp t2 v
      -> TypedExp t2 v
      -> Either ExpressionError t3
    applyBinary op e1 e2 = op <$> eval e1 <*> eval e2
    eval
      :: (Typeable t1)
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

as x scope = AS (Expression x) scope

sColumn :: t -> TypedExp String t
sColumn n = Column n
iColumn :: t -> TypedExp Int t
iColumn n = Column n
bColumn :: t -> TypedExp Bool t
bColumn n = Column n
rColumn :: t -> TypedExp Double t
rColumn n = Column n
colString = Expression . sColumn
colBool = Expression . bColumn
colReal = Expression . rColumn
colInt = Expression . iColumn
