module Conjure.Process.Enumerate ( enumerateDomain, enumerateInConstant ) where

import Conjure.Prelude
import Conjure.Language.AdHoc
import Conjure.Language.Domain
import Conjure.Language.Constant
import Conjure.Language.AbstractLiteral
import Conjure.Language.Pretty


enumerateDomain :: (MonadFail m, Pretty r) => Domain r Constant -> m [Constant]
enumerateDomain DomainBool = return [ConstantBool False, ConstantBool True]
enumerateDomain (DomainInt rs) = concatMapM enumerateRange rs
enumerateDomain (DomainEnum _dName (Just rs) _mp) = concatMapM enumerateRange rs
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
enumerateDomain (DomainSet _ (SetAttr sizeAttr) innerDom) = do
    inners <- enumerateDomain innerDom
    sizes  <- case sizeAttr of
        SizeAttr_None -> return [0 .. genericLength inners]
        SizeAttr_Size (ConstantInt a) -> return [a]
        SizeAttr_MinSize (ConstantInt a) -> return [a .. genericLength inners]
        SizeAttr_MaxSize (ConstantInt a) -> return [0 .. a]
        SizeAttr_MinMaxSize (ConstantInt a) (ConstantInt b) -> return [a .. b]
        _ -> fail $ "sizeAttr, not an int:" <+> pretty sizeAttr
    return
        [ ConstantAbstract (AbsLitSet vals)
        | s <- sizes
        , vals <- replicateM (fromInteger s) inners
        , sorted vals
        ]
enumerateDomain (DomainFunction _ attr DomainBool innerTo) | attr == def = do
    inners <- enumerateDomain innerTo
    let outEmpty     = [ ConstantAbstract (AbsLitFunction []) ]
    let outOnlyFalse = [ ConstantAbstract (AbsLitFunction [ (ConstantBool False, x) ])
                       | x <- inners ]
    let outOnlyTrue  = [ ConstantAbstract (AbsLitFunction [ (ConstantBool True , x) ])
                       | x <- inners ]
    inners2 <- enumerateDomain innerTo
    let outBoth      = [ ConstantAbstract (AbsLitFunction [ (ConstantBool False, x)
                                                          , (ConstantBool True , y) ])
                       | x <- inners
                       , y <- inners2
                       ]
    return $ outEmpty
          ++ outOnlyFalse
          ++ outOnlyTrue
          ++ outBoth
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
    ConstantAbstract (AbsLitMatrix _  xs) -> return xs
    ConstantAbstract (AbsLitSet       xs) -> return xs
    ConstantAbstract (AbsLitMSet      xs) -> return xs
    ConstantAbstract (AbsLitFunction  xs) -> return [ ConstantAbstract (AbsLitTuple [i,j]) | (i,j) <- xs ]
    ConstantAbstract (AbsLitSequence  xs) -> return [ ConstantAbstract (AbsLitTuple [i,j])
                                                    | (i',j) <- zip allNats xs
                                                    , let i = fromInt i'
                                                    ]
    ConstantAbstract (AbsLitRelation  xs) -> return $ map (ConstantAbstract . AbsLitTuple) xs
    ConstantAbstract (AbsLitPartition xs) -> return $ map (ConstantAbstract . AbsLitSet) xs
    TypedConstant c _                     -> enumerateInConstant c
    _ -> fail $ vcat [ "enumerateInConstant"
                     , "constant:" <+> pretty constant
                     ]
