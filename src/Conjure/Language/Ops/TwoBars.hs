{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances #-}

module Conjure.Language.Ops.TwoBars where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpTwoBars x = OpTwoBars x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpTwoBars x)
instance Hashable  x => Hashable  (OpTwoBars x)
instance ToJSON    x => ToJSON    (OpTwoBars x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpTwoBars x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpTwoBars x) where
    typeOf p@(OpTwoBars a) = do
        ty <- typeOf a
        case ty of
            TypeInt{}      -> return ()
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            _              -> raiseTypeError p
        return TypeInt

instance Pretty x => DomainOf (OpTwoBars x) x where
    domainOf op = na $ "evaluateOp{OpTwoBars}:" <++> pretty op

instance EvaluateOp OpTwoBars where
    evaluateOp (OpTwoBars x) =
        case x of
            -- absolute value
            ConstantInt y                        -> return $ ConstantInt $ abs y

            -- cardinality of a constant
            ConstantAbstract (AbsLitSet xs)      -> return $ ConstantInt $ genericLength $ nub xs
            ConstantAbstract (AbsLitMSet xs)     -> return $ ConstantInt $ genericLength       xs
            ConstantAbstract (AbsLitFunction xs) -> return $ ConstantInt $ genericLength $ nub xs

            -- cardinality of a domain
            DomainInConstant (DomainInt rs)      -> ConstantInt . genericLength <$> rangesInts rs

            _ -> na $ "evaluateOp OpTwoBars" <+> pretty (show x)

instance SimplifyOp OpTwoBars where
    simplifyOp _ _ = na "simplifyOp{OpTwoBars}"

instance Pretty x => Pretty (OpTwoBars x) where
    prettyPrec _ (OpTwoBars a) = "|" <> pretty a <> "|"
