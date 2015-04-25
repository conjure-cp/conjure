{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE DeriveGeneric, DeriveDataTypeable, DeriveFunctor, DeriveTraversable, DeriveFoldable #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE UndecidableInstances #-}

module Conjure.Language.Domain.Monoid where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Expression.Op
import Conjure.Language.AdHoc
import Conjure.Language.Pretty
import Conjure.Language.Lenses

-- containers
import Data.Set as S ( empty, union )


instance
    ( ExpressionLike x
    , Op x :< x
    , Pretty x
    , Pretty r
    , Default r
    ) => Monoid (Domain r x) where
    mempty = DomainAny "mempty" TypeAny
    mappend DomainAny{} d = d
    mappend d DomainAny{} = d
    mappend DomainBool DomainBool = DomainBool
    mappend (DomainInt r1) (DomainInt r2) = DomainInt (mappend r1 r2)
    mappend (DomainTuple xs) (DomainTuple ys)
        | length xs == length ys
        = DomainTuple (zipWith mappend xs ys)
    mappend (DomainMatrix x1 x2) (DomainMatrix y1 y2)
        = DomainMatrix (mappend x1 y1) (mappend x2 y2)
    mappend (DomainSet _ _ x) (DomainSet _ _ y)
        = DomainSet def def (mappend x y)
    mappend (DomainMSet _ _ x) (DomainMSet _ _ y)
        = DomainMSet def def (mappend x y)
    mappend (DomainFunction _ _ x1 x2) (DomainFunction _ _ y1 y2)
        = DomainFunction def def (mappend x1 y1) (mappend x2 y2)
    mappend (DomainSequence _ attrX x) (DomainSequence _ attrY y)
        = DomainSequence def (mappend attrX attrY) (mappend x y)
    mappend (DomainRelation _ _ xs) (DomainRelation _ _ ys)
        | length xs == length ys
        = DomainRelation def def (zipWith mappend xs ys)
    mappend (DomainPartition _ _ x) (DomainPartition _ _ y)
        = DomainPartition def def (mappend x y)
    mappend d1 d2 = bug $ vcat ["Domain.mappend", pretty d1, pretty d2]


instance
    ( ExpressionLike x
    , Op x :< x
    , Pretty x
    ) => Monoid (SetAttr x) where
    mempty = SetAttr mempty
    mappend (SetAttr a) (SetAttr b) = SetAttr (mappend a b)


instance
    ( ExpressionLike x
    , Op x :< x
    , Pretty x
    ) => Monoid (SizeAttr x) where
    mempty = def
    mappend SizeAttr_None s = s
    mappend s SizeAttr_None = s
    mappend a b = SizeAttr_MinMaxSize
                        (make opMin (fromList [minA, minB]))
                        (make opMax (fromList [maxA, maxB]))
        where
            (minA, maxA) = getMinMax a
            (minB, maxB) = getMinMax b
            getMinMax p = case p of
                SizeAttr_None -> bug "Monoid SizeAttr"
                SizeAttr_Size x -> (x,x)
                SizeAttr_MinSize x -> (x,x)
                SizeAttr_MaxSize x -> (x,x)
                SizeAttr_MinMaxSize x y -> (x,y)


instance
    ( ExpressionLike x
    , Op x :< x
    , Pretty x
    ) => Monoid (MSetAttr x) where
    mempty = MSetAttr mempty mempty
    mappend (MSetAttr a1 a2) (MSetAttr b1 b2) = MSetAttr (mappend a1 b1) (mappend a2 b2)


instance
    ( ExpressionLike x
    , Op x :< x
    , Pretty x
    ) => Monoid (OccurAttr x) where
    mempty = OccurAttr_None
    mappend OccurAttr_None s = s
    mappend s OccurAttr_None = s
    mappend a b = OccurAttr_MinMaxOccur
                        (make opMin (fromList [minA, minB]))
                        (make opMax (fromList [maxA, maxB]))
        where
            (minA, maxA) = getMinMax a
            (minB, maxB) = getMinMax b
            getMinMax p = case p of
                OccurAttr_None -> bug "Monoid OccurAttr"
                OccurAttr_MinOccur x -> (x,x)
                OccurAttr_MaxOccur x -> (x,x)
                OccurAttr_MinMaxOccur x y -> (x,y)


instance
    ( ExpressionLike x
    , Op x :< x
    , Pretty x
    ) => Monoid (FunctionAttr x) where
    mempty = FunctionAttr mempty mempty mempty
    mappend (FunctionAttr a1 a2 a3) (FunctionAttr b1 b2 b3) =
        FunctionAttr (mappend a1 b1) (mappend a2 b2) (mappend a3 b3)


instance Monoid PartialityAttr where
    mempty = PartialityAttr_Partial
    mappend PartialityAttr_Partial _ = PartialityAttr_Partial
    mappend _ PartialityAttr_Partial = PartialityAttr_Partial
    mappend PartialityAttr_Total PartialityAttr_Total = PartialityAttr_Total


instance Monoid JectivityAttr where
    mempty = JectivityAttr_None
    mappend x y | x == y = x
    mappend _ _ = bug "mappend JectivityAttr_Injective"


instance
    ( ExpressionLike x
    , Op x :< x
    , Pretty x
    ) => Monoid (SequenceAttr x) where
    mempty = SequenceAttr mempty mempty
    mappend (SequenceAttr a1 a2) (SequenceAttr b1 b2) =
        SequenceAttr (mappend a1 b1) (mappend a2 b2)


instance
    ( ExpressionLike x
    , Op x :< x
    , Pretty x
    ) => Monoid (RelationAttr x) where
    mempty = RelationAttr mempty mempty
    mappend (RelationAttr a1 a2) (RelationAttr b1 b2) =
        RelationAttr (mappend a1 b1) (mappend a2 b2)


instance Monoid BinaryRelationAttrs where
    mempty = BinaryRelationAttrs S.empty
    mappend (BinaryRelationAttrs a) (BinaryRelationAttrs b) = BinaryRelationAttrs (S.union a b)


instance
    ( ExpressionLike x
    , Op x :< x
    , Pretty x
    ) => Monoid (PartitionAttr x) where
    mempty = PartitionAttr mempty mempty False
    mappend (PartitionAttr a1 a2 a3) (PartitionAttr b1 b2 b3) =
        PartitionAttr (mappend a1 b1) (mappend a2 b2) (a3 || b3)

