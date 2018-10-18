{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Div where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpDiv x = OpDiv x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpDiv x)
instance Hashable  x => Hashable  (OpDiv x)
instance ToJSON    x => ToJSON    (OpDiv x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpDiv x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpDiv x) where
    opLexeme _ = L_Div

instance (TypeOf x, Pretty x) => TypeOf (OpDiv x) where
    typeOf p@(OpDiv a b) = intToIntToInt p a b

instance EvaluateOp OpDiv where
    evaluateOp p | any isUndef (childrenBi p) = return $ mkUndef (TypeInt Nothing) $ "Has undefined children:" <+> pretty p
    evaluateOp p@(OpDiv x y)
        | y /= 0    = ConstantInt Nothing <$> (div <$> intOut "div x" x <*> intOut "div y" y)
        | otherwise = return $ mkUndef (TypeInt Nothing) $ "division by zero:" <+> pretty p

instance SimplifyOp OpDiv x where
    simplifyOp _ = na "simplifyOp{OpDiv}"

instance Pretty x => Pretty (OpDiv x) where
    prettyPrec prec op@(OpDiv a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpDiv x) where
    varSymBreakingDescription (OpDiv a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpDiv")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        ]
