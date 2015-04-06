{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.TwoBars where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Language.DomainSizeOf

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


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
            TypeInt{}       -> return ()
            TypeSet{}       -> return ()
            TypeMSet{}      -> return ()
            TypeFunction{}  -> return ()
            TypeSequence{}  -> return ()
            TypeRelation{}  -> return ()
            TypePartition{} -> return ()
            _               -> raiseTypeError p
        return TypeInt

instance (Pretty x, TypeOf x) => DomainOf (OpTwoBars x) x where
    domainOf op = mkDomainAny ("OpTwoBars:" <++> pretty op) <$> typeOf op

instance EvaluateOp OpTwoBars where
    evaluateOp (OpTwoBars x) =
        case x of
            -- absolute value
            ConstantInt y                        -> return $ ConstantInt $ abs y

            -- cardinality of a constant
            ConstantAbstract (AbsLitSet       xs) -> return $ ConstantInt $ genericLength $ nub          xs
            ConstantAbstract (AbsLitMSet      xs) -> return $ ConstantInt $ genericLength                xs
            ConstantAbstract (AbsLitFunction  xs) -> return $ ConstantInt $ genericLength $ nub          xs
            ConstantAbstract (AbsLitSequence  xs) -> return $ ConstantInt $ genericLength                xs
            ConstantAbstract (AbsLitRelation  xs) -> return $ ConstantInt $ genericLength $ nub          xs
            ConstantAbstract (AbsLitPartition xs) -> return $ ConstantInt $ genericLength $ nub $ concat xs

            -- cardinality of a domain
            DomainInConstant (DomainInt rs)      -> ConstantInt . genericLength <$> rangesInts rs
            DomainInConstant dom                 -> domainSizeOf dom

            _ -> na $ "evaluateOp OpTwoBars" <+> pretty (show x)

instance SimplifyOp OpTwoBars x where
    simplifyOp _ = na "simplifyOp{OpTwoBars}"

instance Pretty x => Pretty (OpTwoBars x) where
    prettyPrec _ (OpTwoBars a) = "|" <> pretty a <> "|"

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpTwoBars x) where
    varSymBreakingDescription (OpTwoBars a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpTwoBars")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
