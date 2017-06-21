{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Negate where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpNegate x = OpNegate x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpNegate x)
instance Hashable  x => Hashable  (OpNegate x)
instance ToJSON    x => ToJSON    (OpNegate x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpNegate x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpNegate x) where
    typeOf (OpNegate a) = do TypeInt <- typeOf a ; return TypeInt

instance EvaluateOp OpNegate where
    evaluateOp p | any isUndef (childrenBi p) = return $ mkUndef TypeInt $ "Has undefined children:" <+> pretty p
    evaluateOp (OpNegate x) = ConstantInt . negate <$> intOut "OpNegate" x

instance SimplifyOp OpNegate x where
    simplifyOp _ = na "simplifyOp{OpNegate}"

instance Pretty x => Pretty (OpNegate x) where
    prettyPrec prec (OpNegate a) = parensIf (prec > 750) ("-" <> prettyPrec 751 a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpNegate x) where
    varSymBreakingDescription (OpNegate a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpNegate")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
