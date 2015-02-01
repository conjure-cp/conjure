{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Ops.RelationProj where

import Conjure.Prelude
import Conjure.Language.Ops.Common


data OpRelationProj x = OpRelationProj x [Maybe x]      -- Nothing represents an _
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpRelationProj x)
instance Hashable  x => Hashable  (OpRelationProj x)
instance ToJSON    x => ToJSON    (OpRelationProj x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpRelationProj x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpRelationProj x) where
    typeOf p@(OpRelationProj r xs) = do
        TypeRelation ts' <- typeOf r
        let loop [] [] = return []
            loop (Nothing:is) (t:ts) = (t:) <$> loop is ts
            loop (Just i :is) (t:ts) = do
                tyI <- typeOf i
                if typesUnify [tyI,t]
                    then loop is ts
                    else raiseTypeError p
            loop _ _ = raiseTypeError p
        TypeRelation <$> loop xs ts'

instance EvaluateOp OpRelationProj where
    evaluateOp (OpRelationProj (ConstantAbstract (AbsLitRelation xss)) mas) =
        return $ ConstantAbstract $ AbsLitRelation
            [ xsProject
            | xs <- xss
            , let xsProject   = [ x
                                | (x, Nothing) <- zip xs mas
                                ]
            , let xsCondition = [ x == y
                                | (x, Just y ) <- zip xs mas
                                ]
            , and xsCondition
            ]
    evaluateOp op = na $ "evaluateOp{OpRelationProj}:" <++> pretty (show op)

instance Pretty x => Pretty (OpRelationProj x) where
    prettyPrec _ (OpRelationProj a bs) = pretty a <> prettyList prParens "," (map pr bs)
        where pr Nothing = "_"
              pr (Just b) = pretty b
