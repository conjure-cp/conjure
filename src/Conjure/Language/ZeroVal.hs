{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Conjure.Language.ZeroVal ( zeroVal ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty


class ZeroVal a where
    zeroVal :: MonadFail m => a -> m Constant

instance Pretty r => ZeroVal (Domain r Constant) where

    zeroVal DomainBool = return $ ConstantBool False
    zeroVal (DomainInt []) = return $ ConstantInt 0
    zeroVal (DomainInt (r:_)) = zeroVal r
    zeroVal (DomainTuple ds) = ConstantAbstract . AbsLitTuple <$> mapM zeroVal ds
    zeroVal (DomainMatrix index inner) = do
        z  <- zeroVal inner
        is <- case index of
                DomainInt rs -> rangesInts rs
                _ -> fail $ "Matrix indexed by a domain that isn't int:" <+> pretty index
        return $ ConstantAbstract $ AbsLitMatrix index $ replicate (length is) z
    zeroVal d@(DomainSet _ (SetAttr attrs) inner) = do
        let returnInt (ConstantInt x) = return x
            returnInt _ = fail $ "Attribute expected to be an int in:" <+> pretty d
        let getMin SizeAttr_None = return 0
            getMin (SizeAttr_Size x) = returnInt x
            getMin (SizeAttr_MinSize x) = returnInt x
            getMin (SizeAttr_MaxSize _) = return 0
            getMin (SizeAttr_MinMaxSize x _) = returnInt x
        z <- zeroVal inner
        minSize <- getMin attrs
        return $ ConstantAbstract $ AbsLitSet $ replicate minSize z

    -- zeroVal (DomainSet       _ attr d) = DomainSet () attr (forgetRepr d)
    -- zeroVal (DomainMSet      _ attr d) = DomainMSet () attr (forgetRepr d)
    -- zeroVal (DomainFunction  _ attr d1 d2) = DomainFunction () attr (forgetRepr d1) (forgetRepr d2)
    -- zeroVal (DomainRelation  _ attr ds) = DomainRelation () attr (map forgetRepr ds)
    -- zeroVal (DomainPartition _ attr d) = DomainPartition () attr (forgetRepr d)
    -- zeroVal (DomainOp op ds) = DomainOp op (map forgetRepr ds)
    -- zeroVal (DomainHack a) = DomainHack a
    zeroVal d = fail $ "No default value for domain:" <+> pretty d          -- BUG?

instance ZeroVal (Range Constant) where
    zeroVal RangeOpen = fail "No default value for an open range."        -- BUG?
    zeroVal (RangeSingle x) = return x
    zeroVal (RangeLowerBounded x) = return x
    zeroVal (RangeUpperBounded x) = return x
    zeroVal (RangeBounded x _) = return x

