{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Factorial where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpFactorial x = OpFactorial x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpFactorial x)
instance Hashable  x => Hashable  (OpFactorial x)
instance ToJSON    x => ToJSON    (OpFactorial x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFactorial x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpFactorial x) where
    typeOf (OpFactorial a) = do TypeInt NoTag <- typeOf a ; return (TypeInt NoTag)

instance EvaluateOp OpFactorial where
    evaluateOp p | any isUndef (childrenBi p) = return $ mkUndef (TypeInt NoTag) $ "Has undefined children:" <+> pretty p
    evaluateOp (OpFactorial x) = ConstantInt NoTag . product . enumFromTo 1 <$> intOut "factorial" x

instance SimplifyOp OpFactorial x where
    simplifyOp _ = na "simplifyOp{OpFactorial}"

instance Pretty x => Pretty (OpFactorial x) where
    prettyPrec _ (OpFactorial a) = "factorial" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpFactorial x) where
    varSymBreakingDescription (OpFactorial a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpFactorial")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
