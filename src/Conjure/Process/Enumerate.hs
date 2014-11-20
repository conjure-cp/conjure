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
enumerateDomain d = bug $ "enumerateDomain:" <+> pretty d

enumerateRange :: Range Constant -> [Constant]
enumerateRange (RangeSingle x) = [x]
enumerateRange (RangeBounded (ConstantInt x) (ConstantInt y)) = map ConstantInt [x..y]
enumerateRange RangeBounded{} = bug "enumerateRange RangeBounded"
enumerateRange RangeOpen{} = bug "enumerateRange RangeOpen"
enumerateRange RangeLowerBounded{} = bug "enumerateRange RangeLowerBounded"
enumerateRange RangeUpperBounded{} = bug "enumerateRange RangeUpperBounded"

enumerateInConstant :: Constant -> [Constant]
enumerateInConstant constant = case constant of
    ConstantAbstract (AbsLitSet      xs) -> xs
    ConstantAbstract (AbsLitMSet     xs) -> xs
    ConstantAbstract (AbsLitRelation xs) -> map (ConstantAbstract . AbsLitTuple) xs
    _ -> bug $ vcat [ "enumerateInConstant"
                    , "constant:" <+> pretty constant
                    ]
