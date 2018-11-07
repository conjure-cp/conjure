{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Mod where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpMod x = OpMod x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpMod x)
instance Hashable  x => Hashable  (OpMod x)
instance ToJSON    x => ToJSON    (OpMod x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpMod x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpMod x) where
    opLexeme _ = L_Mod

instance (TypeOf x, Pretty x) => TypeOf (OpMod x) where
    typeOf p@(OpMod a b) = intToIntToInt p a b

instance EvaluateOp OpMod where
    evaluateOp p | any isUndef (childrenBi p) =
        return $ mkUndef (TypeInt AnyTag) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpMod x y)
        | y /= 0    = ConstantInt NoTag <$> (mod <$> intOut "mod x" x <*> intOut "mod y" y)
        | otherwise = return $ mkUndef (TypeInt AnyTag) $ "modulo zero:" <+> pretty p

instance SimplifyOp OpMod x where
    simplifyOp _ = na "simplifyOp{OpMod}"

instance Pretty x => Pretty (OpMod x) where
    prettyPrec prec op@(OpMod a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpMod x) where
    varSymBreakingDescription (OpMod a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpMod")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
