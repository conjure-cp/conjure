{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Pow where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpPow x = OpPow x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpPow x)
instance Hashable  x => Hashable  (OpPow x)
instance ToJSON    x => ToJSON    (OpPow x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpPow x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpPow x) where
    opLexeme _ = L_Pow

instance (TypeOf x, Pretty x) => TypeOf (OpPow x) where
  typeOf p@(OpPow a b) = do
    ta <- typeOf a
    tb <- typeOf b
    case (ta, tb) of
      (TypeInt NoTag, TypeInt NoTag) -> intToIntToInt p a b
      _ -> raiseTypeError p

instance EvaluateOp OpPow where
    evaluateOp p | any isUndef (childrenBi p) =
        return $ mkUndef (TypeInt AnyTag) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpPow x y)
        | y >= 0    = ConstantInt NoTag <$> ((^) <$> intOut "pow x" x <*> intOut "pow y" y)
        | otherwise = return $ mkUndef (TypeInt AnyTag) $ "negative exponent:" <+> pretty p

instance SimplifyOp OpPow x where
    simplifyOp _ = na "simplifyOp{OpPow}"

instance Pretty x => Pretty (OpPow x) where
    prettyPrec prec op@(OpPow a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpPow x) where
    varSymBreakingDescription (OpPow a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpPow")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
