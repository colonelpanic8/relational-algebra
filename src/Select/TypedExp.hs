{-|
Module      : Select.Expression
Description : Data types used for ALTER queries
Copyright   : Ivan Malison
Maintainer  : IvanMalison@gmail.com
Stability   : experimental
Portability : POSIX

Data types used for SQL expressions
-}

{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE TypeSynonymInstances #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE StandaloneDeriving #-}

module Select.TypedExp
  ( AnyExpression(..)
  , As
  , BinaryBuilder
  , Expression(..)
  , ExpressionError(..)
  , Named(..)
  , STypeRep(..)
  , STypeable
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
  , sTypeOf
  , stringSType
  , stringType
  , typeOfExpression
  , writeValue
  ) where

import Control.Error.Safe
import Data.Maybe
import Data.Proxy
import Data.Typeable
import Select.Expression (Named(..), As)
import Text.Read
import Unsafe.Coerce

class (Read t, Show t, Eq t, Typeable t) => STypeable t
data STypeRep = forall t. STypeable t => STypeRep (Proxy t)
data Value = forall t. STypeable t => Value t
data AnyExpression v = forall t. STypeable t => AnyExpression (Expression t v)

instance Show STypeRep where
  show (STypeRep p) = showProxy p

showProxy :: Typeable t => Proxy t -> String
showProxy p = show $ typeRep p

instance Show Value where
  show (Value v) = show v

instance Eq STypeRep where
  (STypeRep p1) == (STypeRep p2) = typeRep p1 == typeRep p2

instance Eq Value where
  (Value v) == v2 = Just v == fromValue v2

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

stringSType :: STypeRep
stringSType = STypeRep (Proxy :: Proxy String)
stringType :: TypeRep
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

typeOfExpression :: forall t v. STypeable t => Expression t v -> STypeRep
typeOfExpression _ = STypeRep (Proxy :: Proxy t)

data ExpressionError v = TypeError v [(v, Value)] | BindingError v [(v, Value)]

deriving instance Show v => Show (ExpressionError v)

data Expression t v where
  Literal :: STypeable t => t -> Expression t v
  Column :: STypeable t => v -> Expression t v
  Not :: Expression Bool v -> Expression Bool v
  And :: Expression Bool v -> Expression Bool v -> Expression Bool v
  Or  :: Expression Bool v -> Expression Bool v -> Expression Bool v
  Equ :: (STypeable t) => Expression t v -> Expression t v -> Expression Bool v
  Neq :: (STypeable t) => Expression t v -> Expression t v -> Expression Bool v
  Gt  :: (STypeable t, Ord t) => Expression t v -> Expression t v -> Expression Bool v
  Gte :: (STypeable t, Ord t) => Expression t v -> Expression t v -> Expression Bool v
  Lt  :: (STypeable t, Ord t) => Expression t v -> Expression t v -> Expression Bool v
  Lte :: (STypeable t, Ord t) => Expression t v -> Expression t v -> Expression Bool v
  Neg :: (STypeable t, Num t) => Expression t v -> Expression t v
  Add :: (STypeable t, Num t) => Expression t v -> Expression t v -> Expression t v
  Sub :: (STypeable t, Num t) => Expression t v -> Expression t v -> Expression t v
  Mul :: (STypeable t, Num t) => Expression t v -> Expression t v -> Expression t v
  Div :: (STypeable t, Fractional t) => Expression t v -> Expression t v -> Expression t v
  Mod :: (STypeable t, Integral t) => Expression t v -> Expression t v -> Expression t v

type BinaryBuilder t rt v = (Expression t v -> Expression t v -> Expression rt v)

evaluateExpression
  :: forall v t. (STypeable t, Eq v, Show v)
  => [(v, Value)] -> Expression t v -> Either (ExpressionError v) t
evaluateExpression bindings expr =
  case expr of
    Literal v -> Right v
    Column name -> do
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
      -> Expression t2 v
      -> Expression t2 v
      -> Either (ExpressionError v) t3
    applyBinary op e1 e2 = op <$> eval e1 <*> eval e2
    eval
      :: (STypeable t1)
      => Expression t1 v -> Either (ExpressionError v) t1
    eval = evaluateExpression bindings

as ::
  STypeable t =>
  Expression t v -> scope -> Named scope (AnyExpression v)
as x = AS (AnyExpression x)

sColumn :: t -> Expression String t
sColumn = Column
iColumn :: t -> Expression Int t
iColumn = Column
bColumn :: t -> Expression Bool t
bColumn = Column
rColumn :: t -> Expression Double t
rColumn = Column
colString :: v -> AnyExpression v
colString = AnyExpression . sColumn
colBool :: v -> AnyExpression v
colBool = AnyExpression . bColumn
colReal :: v -> AnyExpression v
colReal = AnyExpression . rColumn
colInt :: v -> AnyExpression v
colInt = AnyExpression . iColumn
