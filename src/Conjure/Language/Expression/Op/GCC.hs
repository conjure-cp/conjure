{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable, ViewPatterns #-}

module Conjure.Language.Expression.Op.GCC where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpGCC x = OpGCC x  -- the "primary" matrix
                     x  -- the "values" matrix, list of values we are interested in (< CatDecision)
                     x  -- the "counts" matrix
                        -- the index of the counts matrix is identical to that of the values
                        -- forAll i . freq(primary, values[i]) = counts[i]
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpGCC x)
instance Hashable  x => Hashable  (OpGCC x)
instance ToJSON    x => ToJSON    (OpGCC x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpGCC x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpGCC x) where
    typeOf p@(OpGCC primary values counts) = do
        tyPrimary <- typeOf primary
        tyValues  <- typeOf values
        tyCounts  <- typeOf counts
        case (tyPrimary, tyValues, tyCounts) of
            (TypeMatrix _ tyPrimaryI, TypeMatrix _ tyValuesI, TypeMatrix _ TypeInt)
                | typeUnify tyPrimaryI tyValuesI -> return TypeBool
            _ -> raiseTypeError p

instance EvaluateOp OpGCC where
    evaluateOp (OpGCC (viewConstantMatrix -> Just (_, primaries))
                      (viewConstantMatrix -> Just (_, values))
                      (viewConstantMatrix -> Just (_, counts)) ) =
        return $ ConstantBool $ and [ ConstantInt (sum [ 1 | prim <- primaries, prim == val ]) == count
                                    | (val, count) <- zip values counts
                                    ]
    evaluateOp op = na $ "evaluateOp{OpGCC}:" <++> pretty (show op)

instance SimplifyOp OpGCC x where
    simplifyOp _ = na "simplifyOp{OpGCC}"

instance Pretty x => Pretty (OpGCC x) where
    prettyPrec _ (OpGCC a b c) = "gcc" <> prettyList prParens "," [a, b, c]

instance (VarSymBreakingDescription x, ExpressionLike x) => VarSymBreakingDescription (OpGCC x) where
    varSymBreakingDescription (OpGCC a b c) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpGCC")
        , ("children", JSON.Array $ V.fromList $ map varSymBreakingDescription [a,b,c])
        ]
