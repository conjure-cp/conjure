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
        tyPrimaryI <- case tyPrimary of
                        TypeMatrix _ i -> return i
                        _ -> raiseTypeError $ vcat
                            [ pretty p
                            , "Unexpected type for the 1st argument:" <++> pretty tyPrimary
                            ]
        tyValuesI  <- case tyValues of
                        TypeMatrix _ i -> return i
                        TypeList     i -> return i
                        _ -> raiseTypeError $ vcat
                            [ pretty p
                            , "Unexpected type for the 2nd argument:" <++> pretty tyValues
                            ]
        case tyCounts of
            TypeMatrix _ TypeInt -> return ()
            _ -> raiseTypeError $ vcat
                [ pretty p
                , "Unexpected type for the 3rd argument:" <++> pretty tyCounts
                ]
        if typeUnify tyPrimaryI tyValuesI
            then return TypeBool
            else raiseTypeError $ vcat [ pretty p
                                       , "The types of the 1st and 2nd arguments do not unify."
                                       , "1st argument has type:" <++> pretty tyPrimary
                                       , "2nd argument has type:" <++> pretty tyValues
                                       , "3rd argument has type:" <++> pretty tyCounts
                                       ]

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
