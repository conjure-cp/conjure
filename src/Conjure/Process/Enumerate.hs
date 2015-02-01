module Conjure.Process.Enumerate ( enumerateDomain, enumerateInConstant ) where

import Conjure.Prelude
import Conjure.Language.Domain
import Conjure.Language.Constant
import Conjure.Language.AbstractLiteral
import Conjure.Language.Pretty


enumerateDomain :: (MonadFail m, Pretty r) => Domain r Constant -> m [Constant]
enumerateDomain DomainBool = return [ConstantBool False, ConstantBool True]
enumerateDomain (DomainInt rs) = concatMapM enumerateRange rs
enumerateDomain (DomainTuple ds) = do
    inners <- mapM enumerateDomain ds
    return $ map (ConstantAbstract . AbsLitTuple) (sequence inners)
enumerateDomain (DomainMatrix (DomainInt indexDom) innerDom) = do
    inners <- enumerateDomain innerDom
    indexInts <- rangesInts indexDom
    return
        [ ConstantAbstract (AbsLitMatrix (DomainInt indexDom) vals)
        | vals <- replicateM (length indexInts) inners
        ]
enumerateDomain (DomainSet _ (SetAttr (SizeAttr_Size (ConstantInt s))) innerDom) = do
    inners <- enumerateDomain innerDom
    return
        [ ConstantAbstract (AbsLitSet vals)
        | vals <- replicateM s inners
        , sorted vals
        ]
enumerateDomain (DomainSet _ (SetAttr sizeAttr) innerDom) = do
    inners <- enumerateDomain innerDom
    sizes  <- case sizeAttr of
        SizeAttr_None -> return [0 .. length inners]
        SizeAttr_Size (ConstantInt a) -> return [a]
        SizeAttr_MinSize (ConstantInt a) -> return [a .. length inners]
        SizeAttr_MaxSize (ConstantInt a) -> return [0 .. a]
        SizeAttr_MinMaxSize (ConstantInt a) (ConstantInt b) -> return [a .. b]
        _ -> fail $ "sizeAttr, not an int:" <+> pretty sizeAttr
    return
        [ ConstantAbstract (AbsLitSet vals)
        | s <- sizes
        , vals <- replicateM s inners
        , sorted vals
        ]
enumerateDomain d = fail $ "enumerateDomain:" <+> pretty d

sorted :: Ord a => [a] -> Bool
sorted xs = and $ zipWith (<) xs (tail xs)

enumerateRange :: MonadFail m => Range Constant -> m [Constant]
enumerateRange (RangeSingle x) = return [x]
enumerateRange (RangeBounded (ConstantInt x) (ConstantInt y)) = return $ map ConstantInt [x..y]
enumerateRange RangeBounded{} = fail "enumerateRange RangeBounded"
enumerateRange RangeOpen{} = fail "enumerateRange RangeOpen"
enumerateRange RangeLowerBounded{} = fail "enumerateRange RangeLowerBounded"
enumerateRange RangeUpperBounded{} = fail "enumerateRange RangeUpperBounded"

enumerateInConstant :: MonadFail m => Constant -> m [Constant]
enumerateInConstant constant = case constant of
    ConstantAbstract (AbsLitMatrix _ xs) -> return xs
    ConstantAbstract (AbsLitSet      xs) -> return xs
    ConstantAbstract (AbsLitMSet     xs) -> return xs
    ConstantAbstract (AbsLitRelation xs) -> return $ map (ConstantAbstract . AbsLitTuple) xs
    _ -> fail $ vcat [ "enumerateInConstant"
                     , "constant:" <+> pretty constant
                     ]
