{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Indexing where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpIndexing x = OpIndexing x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpIndexing x)
instance Hashable  x => Hashable  (OpIndexing x)
instance ToJSON    x => ToJSON    (OpIndexing x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpIndexing x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Show x, Pretty x, ExpressionLike x) => TypeOf (OpIndexing x) where
    typeOf (OpIndexing m i) = do
        tyM <- typeOf m
        case tyM of
            TypeMatrix _ inn -> return inn
            TypeTuple inns   -> do
                TypeInt{} <- typeOf i
                iInt <- intOut i
                return (at inns (iInt-1))
            _ -> fail ("Indexing something other than a matrix or a tuple:" <++> vcat [pretty m, pretty tyM])

instance EvaluateOp OpIndexing where
    evaluateOp (OpIndexing m@(ConstantAbstract (AbsLitMatrix (DomainInt index) vals)) (ConstantInt x)) = do
        ty     <- typeOf m
        let isBool = case ty of
                        TypeMatrix _ TypeBool -> True
                        _                     -> False
        indexVals <- valuesInIntDomain index
        case [ v | (i, v) <- zip indexVals vals, i == x ] of
            [v] -> return v
            _ | isBool -> return $ fromBool False
            []  -> return $ mkUndef $ vcat
                    [ "Matrix is not defined at this point:" <+> pretty x
                    , "Matrix value:" <+> pretty m
                    ]
            _   -> return $ mkUndef $ vcat
                    [ "Matrix is multiply defined at this point:" <+> pretty x
                    , "Matrix value:" <+> pretty m
                    ]
    evaluateOp (OpIndexing (ConstantAbstract (AbsLitTuple vals)) (ConstantInt x)) = return (at vals (x-1))
    evaluateOp op = na $ "evaluateOp{OpIndexing}:" <++> pretty (show op)

instance SimplifyOp OpIndexing where
    simplifyOp _ _ = na "simplifyOp{OpIndexing}"

instance Pretty x => Pretty (OpIndexing x) where
    prettyPrec _ (OpIndexing  a b) = pretty a <> prBrackets (pretty b)
