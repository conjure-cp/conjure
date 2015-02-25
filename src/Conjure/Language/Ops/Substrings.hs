{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.Substrings where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpSubstrings x = OpSubstrings x                -- the sequence
                                   (Maybe x)        -- the length of substrings
                                                    -- nothing means all lengths
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubstrings x)
instance Hashable  x => Hashable  (OpSubstrings x)
instance ToJSON    x => ToJSON    (OpSubstrings x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubstrings x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubstrings x) where
    opLexeme _ = L_substrings

instance (TypeOf x, Pretty x) => TypeOf (OpSubstrings x) where
    typeOf p@(OpSubstrings a b) = do
        tyb <- maybe (return TypeInt) typeOf b
        tya <- typeOf a
        case (tya, tyb) of
            (TypeSequence{}, TypeInt) -> return (TypeSet tya)
            _ -> raiseTypeError p

instance EvaluateOp OpSubstrings where
    evaluateOp op = na $ "evaluateOp{OpSubstrings}:" <++> pretty (show op)

instance SimplifyOp OpSubstrings where
    simplifyOp _ _ = na "simplifyOp{OpSubstrings}"

instance Pretty x => Pretty (OpSubstrings x) where
    prettyPrec _ (OpSubstrings a b) = "substrings" <> prettyList prParens "," xs
        where xs = maybe [a] ((a:) . return) b
