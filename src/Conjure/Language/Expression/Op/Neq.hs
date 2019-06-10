{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Neq where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Language.Expression.Op.Eq
import Conjure.Language.Expression.Op.Not

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpNeq x = OpNeq x x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpNeq x)
instance Hashable  x => Hashable  (OpNeq x)
instance ToJSON    x => ToJSON    (OpNeq x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpNeq x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpNeq x) where
    opLexeme _ = L_Neq

instance (TypeOf x, Pretty x) => TypeOf (OpNeq x) where
    typeOf p@(OpNeq a b) = do
        tyA <- typeOf a
        tyB <- typeOf b
        case tyA of
            TypeList{} -> raiseTypeError $ vcat [ pretty p
                                                , "Cannot use a comprehension in an equality expression."
                                                ]
            _ -> return ()
        case tyB of
            TypeList{} -> raiseTypeError $ vcat [ pretty p
                                                , "Cannot use a comprehension in an equality expression."
                                                ]
            _ -> return ()
        sameToSameToBool p a b [] (const True)

instance EvaluateOp OpNeq where
    evaluateOp (OpNeq ConstantUndefined{} _) = return $ fromBool False
    evaluateOp (OpNeq _ ConstantUndefined{}) = return $ fromBool False
    evaluateOp (OpNeq x y) = do
        out <- evaluateOp (OpEq x y)
        evaluateOp (OpNot out)

instance SimplifyOp OpNeq x where
    simplifyOp _ = na "simplifyOp{OpNeq}"

instance Pretty x => Pretty (OpNeq x) where
    prettyPrec prec op@(OpNeq a b) = prettyPrecBinOp prec [op] a b

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpNeq x) where
    varSymBreakingDescription (OpNeq a b) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpNeq")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            , varSymBreakingDescription b
            ])
        , ("symmetricChildren", JSON.Bool True)
        ]
