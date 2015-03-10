{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}

module Conjure.Language.Expression.Op.RelationProj where

import Conjure.Prelude
import Conjure.Language.Expression.Op.Internal.Common
import Conjure.Language.Expression.Op.Image


data OpRelationProj x = OpRelationProj x [Maybe x]      -- Nothing represents an _
    deriving (Eq, Ord, Show, Data, Functor, Traversable, Foldable, Typeable, Generic)

instance Serialize x => Serialize (OpRelationProj x)
instance Hashable  x => Hashable  (OpRelationProj x)
instance ToJSON    x => ToJSON    (OpRelationProj x) where toJSON = genericToJSON jsonOptions
instance FromJSON  x => FromJSON  (OpRelationProj x) where parseJSON = genericParseJSON jsonOptions

instance (TypeOf x, Pretty x) => TypeOf (OpRelationProj x) where
    typeOf p@(OpRelationProj r xs) = do
        tyR <- typeOf r
        case (tyR, xs) of
            (TypeFunction from to, [Just x]) -> do
                xTy <- typeOf x
                if typesUnify [xTy, from]
                    then return to
                    else raiseTypeError p
            (TypeRelation ts', _) -> do
                let xs' = catMaybes xs
                if length xs == length xs'
                    then do -- all Just's
                        let loop [] [] = return TypeBool
                            loop (i:is) (t:ts) = do
                                tyI <- typeOf i
                                if typesUnify [tyI,t]
                                    then loop is ts
                                    else raiseTypeError p
                            loop _ _ = raiseTypeError p
                        loop xs' ts'
                    else do
                        let loop [] [] = return []
                            loop (Nothing:is) (t:ts) = (t:) <$> loop is ts
                            loop (Just i :is) (t:ts) = do
                                tyI <- typeOf i
                                if typesUnify [tyI,t]
                                    then loop is ts
                                    else raiseTypeError p
                            loop _ _ = raiseTypeError p
                        TypeRelation <$> loop xs ts'
            _ -> raiseTypeError p

instance (Pretty x, TypeOf x) => DomainOf (OpRelationProj x) x where
    domainOf op = mkDomainAny ("OpRelationProj:" <++> pretty op) <$> typeOf op

instance EvaluateOp OpRelationProj where
    evaluateOp (OpRelationProj (ConstantAbstract (AbsLitRelation xss)) mas) = do
        let mas' = catMaybes mas
        if length mas == length mas'
            then -- all Just's
                return $ ConstantBool $ mas' `elem` xss
            else
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
    evaluateOp (OpRelationProj f@(ConstantAbstract AbsLitFunction{}) [Just arg]) =
        evaluateOp (OpImage f arg)
    evaluateOp op = na $ "evaluateOp{OpRelationProj}:" <++> pretty (show op)

instance SimplifyOp OpRelationProj x where
    simplifyOp _ = na "simplifyOp{OpRelationProj}"

instance Pretty x => Pretty (OpRelationProj x) where
    prettyPrec _ (OpRelationProj a bs) = pretty a <> prettyList prParens "," (map pr bs)
        where pr Nothing = "_"
              pr (Just b) = pretty b
