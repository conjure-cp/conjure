{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Factorial where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpFactorial x = OpFactorial x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpFactorial x)
instance Hashable  x => Hashable  (OpFactorial x)
instance ToJSON    x => ToJSON    (OpFactorial x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpFactorial x) where parseJSON = genericParseJSON jsonOptions

instance TypeOf x => TypeOf (OpFactorial x) where
    typeOf (OpFactorial a) = do TypeInt <- typeOf a ; return TypeInt

instance EvaluateOp OpFactorial where
    evaluateOp p | any isUndef (universeBi p) = return $ mkUndef TypeInt $ "Has undefined children:" <+> pretty p
    evaluateOp (OpFactorial x) = ConstantInt . product . enumFromTo 1 <$> intOut x

instance SimplifyOp OpFactorial where
    simplifyOp _ _ = na "simplifyOp{OpFactorial}"

instance Pretty x => Pretty (OpFactorial x) where
    prettyPrec _ (OpFactorial a) = "factorial" <> prParens (pretty a)
