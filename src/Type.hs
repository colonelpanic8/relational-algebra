{-# LANGUAGE ScopedTypeVariables #-}

module Type where

import qualified Select.Expression as UE
import qualified Select.Relation as UR
import qualified Select.TypedExp as TE
import qualified Select.TypedRel as TR

import           Control.Error.Safe
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
    UE.And e1 e2 -> construct TE.And e1 e2
    UE.Or e1 e2 -> construct TE.Or e1 e2
    UE.Add e1 e2 ->
      construct
        (TE.Add :: TE.Expression Int v ->
                   TE.Expression Int v ->
                   TE.Expression Int v) e1 e2
  where
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
