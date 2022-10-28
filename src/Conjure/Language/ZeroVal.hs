module Conjure.Language.ZeroVal ( zeroVal, EnumerateDomain ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Process.Enumerate ( EnumerateDomain, enumerateDomain )


zeroVal :: (MonadFailDoc m, EnumerateDomain m, Pretty r) => Domain r Constant -> m Constant
zeroVal DomainBool = return $ ConstantBool False
zeroVal (DomainInt t []) = return $ ConstantInt t 0
zeroVal (DomainInt _ (r:_)) = zeroValR r
zeroVal (DomainTuple ds) = ConstantAbstract . AbsLitTuple <$> mapM zeroVal ds
zeroVal (DomainRecord xs) = do
    values <- forM xs $ \ (nm, dom) -> do
        z <- zeroVal dom
        return (nm, z)
    return $ ConstantAbstract $ AbsLitRecord values
zeroVal (DomainVariant xs@((nm, dom):_)) = do
    z <- zeroVal dom
    return $ ConstantAbstract $ AbsLitVariant (Just [(n, forgetRepr d) | (n,d) <- xs]) nm z
zeroVal (DomainMatrix index inner) = do
    z  <- zeroVal inner
    is <- case index of
            DomainInt _ rs -> rangesInts rs
            _ -> fail $ "Matrix indexed by a domain that isn't int:" <+> pretty index
    return $ ConstantAbstract $ AbsLitMatrix index $ replicate (length is) z
zeroVal d@(DomainSet _ (SetAttr sizeAttr) inner) = do
    z       <- zeroVal inner
    minSize <- getMin d sizeAttr
    return $ ConstantAbstract $ AbsLitSet $ replicate (fromInteger minSize) z
zeroVal d@(DomainSequence _ (SequenceAttr sizeAttr _) inner) = do
    z       <- zeroVal inner
    minSize <- getMin d sizeAttr
    return $ ConstantAbstract $ AbsLitSequence $ replicate (fromInteger minSize) z
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
zeroVal d@(DomainMSet _ (MSetAttr sizeAttr _) inner) = do
    z       <- zeroVal inner
    minSize <- getMin d sizeAttr
    return $ ConstantAbstract $ AbsLitMSet $ replicate (fromInteger minSize) z
zeroVal d@(DomainRelation _ (RelationAttr sizeAttr _) inners) = do
    zs      <- mapM zeroVal inners
    minSize <- getMin d sizeAttr
    return $ ConstantAbstract $ AbsLitRelation $ replicate (fromInteger minSize) zs
zeroVal d@(DomainPartition _ (PartitionAttr numPartsAttr partSizeAttr _) inner) = do
    z        <- zeroVal inner
    minSize1 <- getMin d numPartsAttr
    minSize2 <- getMin d partSizeAttr
    return $ ConstantAbstract $ AbsLitPartition $ replicate (fromInteger minSize1)
                                                            (replicate (fromInteger minSize2) z)
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
returnInt _ (ConstantInt _ x) = return x
returnInt d _ = fail $ "Attribute expected to be an int in:" <+> pretty d
