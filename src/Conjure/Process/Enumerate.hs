module Conjure.Process.Enumerate ( enumerateDomain, enumerateInConstant ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Domain
import Conjure.Language.Constant
import Conjure.Language.AbstractLiteral
import Conjure.Language.Pretty


enumerateDomain :: Pretty r => Domain r Constant -> [Constant]
enumerateDomain DomainBool = [ConstantBool False, ConstantBool True]
enumerateDomain (DomainInt rs) = concatMap enumerateRange rs
enumerateDomain (DomainTuple ds) = map (ConstantAbstract . AbsLitTuple) (mapM enumerateDomain ds)
enumerateDomain (DomainSet _ (SetAttr (SizeAttr_Size (ConstantInt s))) innerDom) =
    [ ConstantAbstract (AbsLitSet vals)
    | let inners = enumerateDomain innerDom
    , vals <- replicateM s inners
    , sorted vals
    ]
enumerateDomain (DomainSet _ (SetAttr sizeAttr) innerDom) =
    [ ConstantAbstract (AbsLitSet vals)
    | let inners = enumerateDomain innerDom
    , let sizes  = case sizeAttr of
            SizeAttr_None -> [0 .. length inners]
            SizeAttr_Size (ConstantInt a) -> [a]
            SizeAttr_MinSize (ConstantInt a) -> [a .. length inners]
            SizeAttr_MaxSize (ConstantInt a) -> [0 .. a]
            SizeAttr_MinMaxSize (ConstantInt a) (ConstantInt b) -> [a .. b]
            _ -> bug $ "sizeAttr, not an int:" <+> pretty sizeAttr
    , s <- sizes
    , vals <- replicateM s inners
    , sorted vals
    ]
enumerateDomain d = bug $ "enumerateDomain:" <+> pretty d

sorted :: Ord a => [a] -> Bool
sorted xs = and $ zipWith (<) xs (tail xs)

enumerateRange :: Range Constant -> [Constant]
enumerateRange (RangeSingle x) = [x]
enumerateRange (RangeBounded (ConstantInt x) (ConstantInt y)) = map ConstantInt [x..y]
enumerateRange RangeBounded{} = bug "enumerateRange RangeBounded"
enumerateRange RangeOpen{} = bug "enumerateRange RangeOpen"
enumerateRange RangeLowerBounded{} = bug "enumerateRange RangeLowerBounded"
enumerateRange RangeUpperBounded{} = bug "enumerateRange RangeUpperBounded"

enumerateInConstant :: Constant -> [Constant]
enumerateInConstant constant = case constant of
    ConstantAbstract (AbsLitMatrix _ xs) -> xs
    ConstantAbstract (AbsLitSet      xs) -> xs
    ConstantAbstract (AbsLitMSet     xs) -> xs
    ConstantAbstract (AbsLitRelation xs) -> map (ConstantAbstract . AbsLitTuple) xs
    _ -> bug $ vcat [ "enumerateInConstant"
                    , "constant:" <+> pretty constant
                    ]
