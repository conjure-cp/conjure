module Conjure.Process.EnumerateDomain ( enumerateDomain ) where

import Conjure.Prelude
import Conjure.Bug
-- import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Constant
import Conjure.Language.Pretty


enumerateDomain :: Pretty r => Domain r Constant -> [Constant]
enumerateDomain DomainBool = [ConstantBool False, ConstantBool True]
enumerateDomain (DomainInt rs) = concatMap enumerateRange rs
enumerateDomain (DomainTuple ds) = map ConstantTuple (mapM enumerateDomain ds)
enumerateDomain d = bug $ "enumerateDomain:" <+> pretty d

enumerateRange :: Range Constant -> [Constant]
enumerateRange (RangeSingle x) = [x]
enumerateRange (RangeBounded (ConstantInt x) (ConstantInt y)) = map ConstantInt [x..y]
enumerateRange RangeBounded{} = bug "enumerateRange RangeBounded"
enumerateRange RangeOpen{} = bug "enumerateRange RangeOpen"
enumerateRange RangeLowerBounded{} = bug "enumerateRange RangeLowerBounded"
enumerateRange RangeUpperBounded{} = bug "enumerateRange RangeUpperBounded"
