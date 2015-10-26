module Conjure.Language.ZeroVal ( zeroVal, EnumerateDomain ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Process.Enumerate ( EnumerateDomain, enumerateDomain )


zeroVal :: (MonadFail m, EnumerateDomain m, Pretty r) => Domain r Constant -> m Constant
zeroVal DomainBool = return $ ConstantBool False
zeroVal (DomainInt []) = return $ ConstantInt 0
zeroVal (DomainInt (r:_)) = zeroValR r
zeroVal (DomainTuple ds) = ConstantAbstract . AbsLitTuple <$> mapM zeroVal ds
-- TODO: add DomainRecord
-- TODO: add DomainVariant
zeroVal (DomainMatrix index inner) = do
    z  <- zeroVal inner
    is <- case index of
            DomainInt rs -> rangesInts rs
            _ -> fail $ "Matrix indexed by a domain that isn't int:" <+> pretty index
    return $ ConstantAbstract $ AbsLitMatrix index $ replicate (length is) z
zeroVal d@(DomainSet _ (SetAttr sizeAttr) inner) = do
    z       <- zeroVal inner
    minSize <- getMin d sizeAttr
    return $ ConstantAbstract $ AbsLitSet $ replicate (fromInteger minSize) z
zeroVal d@(DomainFunction _ (FunctionAttr sizeAttr partialityAttr _) innerFr innerTo) =
    case partialityAttr of
        PartialityAttr_Partial -> do
            minSize <- getMin d sizeAttr
            zFr     <- zeroVal innerFr
            zTo     <- zeroVal innerTo
            return $ ConstantAbstract $ AbsLitFunction $ replicate (fromInteger minSize) (zFr, zTo)
        PartialityAttr_Total   -> do
            froms   <- enumerateDomain (forgetRepr innerFr)
            zTo     <- zeroVal innerTo
            return $ ConstantAbstract $ AbsLitFunction [ (fr, zTo) | fr <- froms ]
-- zeroVal (DomainMSet      _ attr d) = DomainMSet () attr (forgetRepr d)
-- zeroVal (DomainRelation  _ attr ds) = DomainRelation () attr (map forgetRepr ds)
-- zeroVal (DomainPartition _ attr d) = DomainPartition () attr (forgetRepr d)
-- zeroVal (DomainOp op ds) = DomainOp op (map forgetRepr ds)
-- zeroVal (DomainHack a) = DomainHack a
zeroVal d = bug $ "No 'zero' value for domain:" <+> pretty d


zeroValR :: MonadFail m => Range a -> m a
zeroValR RangeOpen = fail "No 'zero' value for an open range."
zeroValR (RangeSingle x) = return x
zeroValR (RangeLowerBounded x) = return x
zeroValR (RangeUpperBounded x) = return x
zeroValR (RangeBounded x _) = return x


getMin :: (MonadFail m, Pretty r, Pretty x) => Domain r x -> SizeAttr Constant -> m Integer
getMin _ SizeAttr_None = return 0
getMin d (SizeAttr_Size x) = returnInt d x
getMin d (SizeAttr_MinSize x) = returnInt d x
getMin _ (SizeAttr_MaxSize _) = return 0
getMin d (SizeAttr_MinMaxSize x _) = returnInt d x


returnInt :: (MonadFail m, Pretty r, Pretty x) => Domain r x -> Constant -> m Integer
returnInt _ (ConstantInt x) = return x
returnInt d _ = fail $ "Attribute expected to be an int in:" <+> pretty d
