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
{-# LANGUAGE StandaloneDeriving #-}

module Select.Expression
  ( As
  , Expression(..)
  , ExpressionError(..)
  , Named(..)
  , STypeRep(..)
  , STypeable
  , Typeable(..)
  , TypedExp(..)
  , Value(..)
  , as
  , bColumn
  , colBool
  , colInt
  , colReal
  , colString
  , evaluateExpression
  , fromValue
  , iColumn
  , rColumn
  , readValue
  , sColumn
  , stringSType
  , stringType
  , typeOfTypedExp
  , sTypeOf
  , writeValue
  ) where

import Control.Error.Safe
import Data.Maybe
import Data.Proxy
import Data.Typeable
import Data.Unique
import System.IO.Unsafe
import Text.Read
import Unsafe.Coerce

class (Read t, Show t, Eq t, Typeable t) => STypeable t
data STypeRep = forall t. STypeable t => STypeRep (Proxy t)
data Value = forall t. STypeable t => Value t
data Expression v = forall t. STypeable t => Expression (TypedExp t v)

instance Show STypeRep where
  show (STypeRep p) = showProxy p

showProxy :: Typeable t => Proxy t -> String
showProxy p = show $ typeRep p

instance Show Value where
  show (Value v) = show v

instance Eq STypeRep where
  (STypeRep p1) == (STypeRep p2) = typeRep p1 == typeRep p2

instance Eq Value where
  (Value v) == v2 = Just v == (fromValue v2)

instance STypeable String
instance STypeable Int
instance STypeable Double
instance STypeable Bool

sTypeOf :: forall v. STypeable v => v -> STypeRep
sTypeOf _ = STypeRep (Proxy :: Proxy v)

readValue :: STypeRep -> String -> Maybe Value
readValue (STypeRep p) s =
  helper p
  where helper :: forall t. STypeable t => Proxy t -> Maybe Value
        helper _ = Value <$> (readMaybe s :: Maybe t)

stringSType = STypeRep (Proxy :: Proxy String)
stringType = typeRep (Proxy :: Proxy String)

writeValue :: Value -> String
writeValue v@(Value t) =
  fromMaybe (show t) $ fromValue v

fromValue :: forall t. STypeable t => Value -> Maybe t
fromValue (Value v) =
  if typeOf v == typeRep (Proxy :: Proxy t) then
    Just $ unsafeCoerce v
  else
    Nothing

typeOfTypedExp :: forall t v. STypeable t => TypedExp t v -> STypeRep
typeOfTypedExp _ = STypeRep (Proxy :: Proxy t)

data ExpressionError v = TypeError v [(v, Value)] | BindingError v [(v, Value)]

deriving instance Show v => Show (ExpressionError v)

data TypedExp t v where
  Literal :: STypeable t => t -> TypedExp t v
  Column :: STypeable t => v -> TypedExp t v
  Not :: TypedExp Bool v -> TypedExp Bool v
  And :: TypedExp Bool v -> TypedExp Bool v -> TypedExp Bool v
  Or  :: TypedExp Bool v -> TypedExp Bool v -> TypedExp Bool v
  Equ :: (STypeable t, Eq t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Neq :: (STypeable t, Eq t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Gt  :: (STypeable t, Ord t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Gte :: (STypeable t, Ord t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Lt  :: (STypeable t, Ord t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Lte :: (STypeable t, Ord t) => TypedExp t v -> TypedExp t v -> TypedExp Bool v
  Neg :: (STypeable t, Num t) => TypedExp t v -> TypedExp t v
  Add :: (STypeable t, Num t) => TypedExp t v -> TypedExp t v -> TypedExp t v
  Sub :: (STypeable t, Num t) => TypedExp t v -> TypedExp t v -> TypedExp t v
  Mul :: (STypeable t, Num t) => TypedExp t v -> TypedExp t v -> TypedExp t v
  Div :: (STypeable t, Fractional t) => TypedExp t v -> TypedExp t v -> TypedExp t v
  Mod :: (STypeable t, Integral t) => TypedExp t v -> TypedExp t v -> TypedExp t v

evaluateExpression
  :: forall v t. (STypeable t, Eq v, Show v)
  => [(v, Value)] -> TypedExp t v -> Either (ExpressionError v) t
evaluateExpression bindings expr =
  case expr of
    Literal v -> Right v
    c@(Column name) -> do
      binding <- justErr (BindingError name bindings) $ lookup name bindings
      justErr (TypeError name bindings) $ fromValue binding
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
      :: (STypeable t2)
      => (t2 -> t2 -> t3)
      -> TypedExp t2 v
      -> TypedExp t2 v
      -> Either (ExpressionError v) t3
    applyBinary op e1 e2 = op <$> eval e1 <*> eval e2
    eval
      :: (STypeable t1)
      => TypedExp t1 v -> Either (ExpressionError v) t1
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
