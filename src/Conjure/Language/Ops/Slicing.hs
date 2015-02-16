{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Slicing where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpSlicing x = OpSlicing x (Maybe x) (Maybe x)
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSlicing x)
instance Hashable  x => Hashable  (OpSlicing x)
instance ToJSON    x => ToJSON    (OpSlicing x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSlicing x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpSlicing x) where
    typeOf p@(OpSlicing m _ _) = do
        ty <- typeOf m
        case ty of
            TypeMatrix{} -> return ()
            TypeList{} -> return ()
            _ -> raiseTypeError p
        return ty

instance EvaluateOp OpSlicing where
    evaluateOp op@(OpSlicing m lb ub) = case m of
        ConstantAbstract (AbsLitMatrix (DomainInt index) vals) -> do
            indexVals <- valuesInIntDomain index
            outVals   <- liftM catMaybes $ forM (zip indexVals vals) $ \ (thisIndex, thisVal) ->
                case lb of
                    Just (ConstantInt lower) | lower > thisIndex -> return Nothing
                    _ -> case ub of
                        Just (ConstantInt upper) | upper < thisIndex -> return Nothing
                        _ -> return $ Just (thisIndex, thisVal)
            let outDomain = DomainInt $ map (RangeSingle . ConstantInt . fst) outVals
            return $ ConstantAbstract $ AbsLitMatrix outDomain (map snd outVals)
        _ -> na $ "evaluateOp{OpSlicing}:" <++> pretty (show op)

instance SimplifyOp OpSlicing where
    simplifyOp _ _ = na "simplifyOp{OpSlicing}"

instance Pretty x => Pretty (OpSlicing x) where
    prettyPrec _ (OpSlicing m a b) = pretty m <> prBrackets (pretty a <> ".." <> pretty b)
