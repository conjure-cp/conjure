{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.Subsequences where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common


data OpSubsequences x = OpSubsequences x            -- the sequence
                                       (Maybe x)    -- the length of subsequences
                                                    -- nothing means all lengths
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpSubsequences x)
instance Hashable  x => Hashable  (OpSubsequences x)
instance ToJSON    x => ToJSON    (OpSubsequences x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpSubsequences x) where parseJSON = genericParseJSON jsonOptions

instance BinaryOperator (OpSubsequences x) where
    opLexeme _ = L_subsequences

instance (TypeOf x, Pretty x) => TypeOf (OpSubsequences x) where
    typeOf p@(OpSubsequences a b) = do
        tyb <- maybe (return TypeInt) typeOf b
        tya <- typeOf a
        case (tya, tyb) of
            (TypeSequence{}, TypeInt) -> return (TypeSet tya)
            _ -> raiseTypeError p

instance DomainOf (OpSubsequences x) x where
    domainOf _ = fail "domainOf{OpSubsequences}"

instance EvaluateOp OpSubsequences where
    evaluateOp op = na $ "evaluateOp{OpSubsequences}:" <++> pretty (show op)

instance SimplifyOp OpSubsequences x where
    simplifyOp _ = na "simplifyOp{OpSubsequences}"

instance Pretty x => Pretty (OpSubsequences x) where
    prettyPrec _ (OpSubsequences a b) = "subsequences" <> prettyList prParens "," xs
        where xs = maybe [a] ((a:) . return) b
