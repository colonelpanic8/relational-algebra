{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes #-}

module Type where

import qualified Select.Expression as UE
import qualified Select.Relation as UR
import qualified Select.TypedExp as TE
import qualified Select.TypedRel as TR

import           Control.Error.Safe
import           Data.Either
import           Data.Proxy
import           Unsafe.Coerce


getColumnOfType :: forall v. v -> TE.STypeRep -> TE.AnyExpression v
getColumnOfType name (TE.STypeRep p) = helper p
  where
    helper
      :: forall t.
         TE.STypeable t
      => Proxy t -> TE.AnyExpression v
    helper _ = TE.AnyExpression (TE.Column name :: TE.Expression t v)

backup a b = if isLeft a then b else a

backups [a] = a
backups (a:as) = if isLeft a then backups as else a

typeExpression
  :: forall v.
     (Eq v, Show v)
  => TR.TypeRequirements v
  -> UE.Expression v
  -> Either TR.RelationError (TE.AnyExpression v)
typeExpression reqs expr =
  case expr of
    UE.Column name ->
      justErr TR.BadExpressionError $ getColumnOfType name <$> lookup name reqs
    UE.LiteralBool b -> Right $ TE.AnyExpression $ TE.Literal b
    UE.And e1 e2 -> construct TE.eand e1 e2
    UE.Or e1 e2 -> construct TE.eor e1 e2
    UE.Add e1 e2 -> constructNumeric TE.eadd e1 e2
    UE.Equ e1 e2 -> constructOrdered TE.eequ e1 e2
    UE.Lt e1 e2 -> constructOrdered TE.elt e1 e2
    UE.Not e1 -> TE.AnyExpression . TE.enot <$> (recurse e1 >>= getExpressionOfType)
    UE.Div e1 e2 -> construct (TE.ediv :: TE.BinaryBuilder Double Double v) e1 e2
  where
    constructNumeric
      :: (forall t. (TE.STypeable t, Num t) => TE.BinaryBuilder t t v)
      -> UE.Expression v
      -> UE.Expression v
      -> Either TR.RelationError (TE.AnyExpression v)
    constructNumeric constructor e1 e2 =
      let intConstructor =
            (constructor :: TE.BinaryBuilder Int Int v)
          realConstructor =
            (constructor :: TE.BinaryBuilder Double Double v)
      in backup
           (construct intConstructor e1 e2)
           (construct realConstructor e1 e2)
    constructOrdered
      :: (forall t. (TE.STypeable t, Ord t) => TE.BinaryBuilder t Bool v)
      -> UE.Expression v
      -> UE.Expression v
      -> Either TR.RelationError (TE.AnyExpression v)
    constructOrdered constructor e1 e2 =
      let intConstructor =
            (constructor :: TE.BinaryBuilder Int Bool v)
          realConstructor =
            (constructor :: TE.BinaryBuilder Double Bool v)
          boolConstructor =
            (constructor :: TE.BinaryBuilder Bool Bool v)
          stringConstructor =
            (constructor :: TE.BinaryBuilder String Bool v)
      in backups
           [ construct intConstructor e1 e2
           , construct realConstructor e1 e2
           , construct boolConstructor e1 e2
           , construct stringConstructor e1 e2
           ]
    recurse = typeExpression reqs
    getExpressionOfType
      :: forall t.
         TE.STypeable t
      => TE.AnyExpression v -> Either TR.RelationError (TE.Expression t v)
    getExpressionOfType (TE.AnyExpression e) =
      if TE.typeOfExpression e == TE.STypeRep (Proxy :: Proxy t)
        then Right $ unsafeCoerce e
        else Left TR.BadExpressionError
    construct
      :: forall t rt.
         (TE.STypeable t, TE.STypeable rt)
      => TE.BinaryBuilder t rt v
      -> UE.Expression v
      -> UE.Expression v
      -> Either TR.RelationError (TE.AnyExpression v)
    construct constructor e1 e2 = do
      te1 <- recurse e1 >>= getExpressionOfType
      te2 <- recurse e2 >>= getExpressionOfType
      return $ TE.AnyExpression $ constructor te1 te2
