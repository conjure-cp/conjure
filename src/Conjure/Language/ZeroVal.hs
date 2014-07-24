{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE OverloadedStrings #-}

module Conjure.Language.ZeroVal ( zeroVal ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty


class ZeroVal a where
    zeroVal :: (Applicative m, MonadError Doc m) => a -> m Constant

instance Pretty r => ZeroVal (Domain r Constant) where

    zeroVal DomainBool = return $ ConstantBool False
    zeroVal (DomainInt []) = return $ ConstantInt 0
    zeroVal (DomainInt (r:_)) = zeroVal r
    zeroVal (DomainTuple ds) = ConstantTuple <$> mapM zeroVal ds
    zeroVal (DomainMatrix index inner) = do
        z  <- zeroVal inner
        is <- case index of
                DomainInt rs -> rangesInts rs
                _ -> throwError $ "Matrix indexed by a domain that isn't int:" <+> pretty index
        return (ConstantMatrix index (replicate (length is) z))
    zeroVal d@(DomainSet _ attrs inner) = do
        let returnInt (ConstantInt x) = return x
            returnInt _ = throwError $ "Attribute expected to be an int in:" <+> pretty d
        let getMin SetAttrNone = return 0
            getMin (SetAttrSize x) = returnInt x
            getMin (SetAttrMinSize x) = returnInt x
            getMin (SetAttrMaxSize _) = return 0
            getMin (SetAttrMinMaxSize x _) = returnInt x
            getMin (SetAttrDotDot as) = getMin as
        z <- zeroVal inner
        minSize <- getMin attrs
        return (ConstantSet (replicate minSize z))

    -- zeroVal (DomainSet       _ attr d) = DomainSet () attr (forgetRepr d)
    -- zeroVal (DomainMSet      _ attr d) = DomainMSet () attr (forgetRepr d)
    -- zeroVal (DomainFunction  _ attr d1 d2) = DomainFunction () attr (forgetRepr d1) (forgetRepr d2)
    -- zeroVal (DomainRelation  _ attr ds) = DomainRelation () attr (map forgetRepr ds)
    -- zeroVal (DomainPartition _ attr d) = DomainPartition () attr (forgetRepr d)
    -- zeroVal (DomainOp op ds) = DomainOp op (map forgetRepr ds)
    -- zeroVal (DomainHack a) = DomainHack a
    zeroVal d = throwError $ "No default value for domain:" <+> pretty d          -- BUG?

instance ZeroVal (Range Constant) where
    zeroVal RangeOpen = throwError "No default value for an open range."        -- BUG?
    zeroVal (RangeSingle x) = return x
    zeroVal (RangeLowerBounded x) = return x
    zeroVal (RangeUpperBounded x) = return x
    zeroVal (RangeBounded x _) = return x

