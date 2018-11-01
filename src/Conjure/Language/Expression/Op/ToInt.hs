{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.ToInt where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common

import qualified Data.Aeson as JSON             -- aeson
import qualified Data.HashMap.Strict as M       -- unordered-containers
import qualified Data.Vector as V               -- vector


data OpToInt x = OpToInt x
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpToInt x)
instance Hashable  x => Hashable  (OpToInt x)
instance ToJSON    x => ToJSON    (OpToInt x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpToInt x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpToInt x) where
    typeOf p@(OpToInt x) = do
        ty <- typeOf x
        case ty of
            TypeBool -> return $ TypeInt NoTag
            _ -> raiseTypeError $ vcat
                [ pretty p
                , "Expected type bool."
                , "But got:" <+> pretty ty
                ]

instance EvaluateOp OpToInt where
    evaluateOp (OpToInt (ConstantBool False)) = return (ConstantInt NoTag 0)
    evaluateOp (OpToInt (ConstantBool True )) = return (ConstantInt NoTag 1)
    evaluateOp (OpToInt ConstantUndefined{})  = return (ConstantInt NoTag 0)
    evaluateOp op = na $ "evaluateOp{OpToInt}:" <++> pretty (show op)

instance SimplifyOp OpToInt x where
    simplifyOp _ = na "simplifyOp{OpToInt}"

instance Pretty x => Pretty (OpToInt x) where
    prettyPrec _ (OpToInt a) = "toInt" <> prParens (pretty a)

instance VarSymBreakingDescription x => VarSymBreakingDescription (OpToInt x) where
    varSymBreakingDescription (OpToInt a) = JSON.Object $ M.fromList
        [ ("type", JSON.String "OpToInt")
        , ("children", JSON.Array $ V.fromList
            [ varSymBreakingDescription a
            ])
        ]
