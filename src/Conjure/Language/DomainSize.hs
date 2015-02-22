{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}

module Conjure.Language.DomainSize
    ( domainSizeConstant
    , valuesInIntDomain
    , domainSizeOf
    , enumNameToInt, enumIntToName
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Ops ( OperatorContainer(..), valuesInIntDomain )
import Conjure.Language.Lenses
import Conjure.Language.Pretty


class DomainSizeOf a x where
    domainSizeOf :: MonadFail m => a -> m x

instance DomainSizeOf Constant Int where
    domainSizeOf = const (return 1)

instance DomainSizeOf (Domain r Constant) Int where
    domainSizeOf = domainSizeConstant

instance DomainSizeOf (Domain r Constant) Constant where
    domainSizeOf = fmap ConstantInt . domainSizeOf

-- instance DomainSizeOf (Domain r Constant) Expression where
--     domainSizeOf = fmap Constant . domainSizeOf

instance DomainSizeOf (Domain HasRepresentation Expression) Expression where
    domainSizeOf (DomainEnum n Nothing _) = return $
        let n' = n `mappend` "_EnumSize"
        in  Reference n' (Just (DeclHasRepr Given n' (DomainInt [])))
    domainSizeOf d = gDomainSizeOf (forgetRepr d)


gDomainSizeOf
    :: forall m x
    . ( MonadFail m
      , ExpressionLike x
      , OperatorContainer x
      , Pretty x
      , Integral x
      )
    => Domain () x -> m x
gDomainSizeOf DomainBool = return 2
gDomainSizeOf (DomainInt [] ) = fail "gDomainSizeOf infinite integer domain"
gDomainSizeOf (DomainInt [r]) = domainSizeOf r
gDomainSizeOf (DomainInt rs ) = make opSum . fromList <$> mapM domainSizeOf rs
gDomainSizeOf (DomainUnnamed _ x) = return x
gDomainSizeOf (DomainTuple []) = fail "gDomainSizeOf: nullary tuple"
gDomainSizeOf (DomainTuple xs) = make opProduct . fromList <$> mapM gDomainSizeOf xs
gDomainSizeOf (DomainRecord xs) = make opProduct . fromList <$> mapM (gDomainSizeOf . snd) xs
gDomainSizeOf (DomainVariant xs) = make opSum . fromList <$> mapM (gDomainSizeOf . snd) xs
gDomainSizeOf (DomainMatrix index inner) = make opPow <$> gDomainSizeOf inner <*> gDomainSizeOf index
gDomainSizeOf (DomainSet _ (SetAttr sizeAttr) inner) = do
    innerSize <- gDomainSizeOf inner
    case sizeAttr of
        SizeAttr_None           -> return (make opPow 2 innerSize)
        SizeAttr_Size size      -> return (nchoosek (make opFactorial) innerSize size)
        SizeAttr_MinSize _      -> return (make opPow 2 innerSize)              -- TODO: can be better
        SizeAttr_MaxSize _      -> return (make opPow 2 innerSize)              -- TODO: can be better
        SizeAttr_MinMaxSize _ _ -> return (make opPow 2 innerSize)              -- TODO: can be better
gDomainSizeOf (DomainMSet _ attrs inner) = do
    innerSize <- gDomainSizeOf inner
    let
        getMaxSize = case attrs of
            MSetAttr (SizeAttr_Size x) _ -> return x
            MSetAttr (SizeAttr_MaxSize x) _ -> return x
            MSetAttr (SizeAttr_MinMaxSize _ x) _ -> return x
            MSetAttr _ (OccurAttr_MaxOccur x) -> return (x * innerSize)
            MSetAttr _ (OccurAttr_MinMaxOccur _ x) -> return (x * innerSize)
            _ -> fail ("gDomainSizeOf.getMaxSize, mset not supported. attributes:" <+> pretty attrs)
        getMaxOccur = case attrs of
            MSetAttr _ (OccurAttr_MaxOccur x) -> return x
            MSetAttr _ (OccurAttr_MinMaxOccur _ x) -> return x
            MSetAttr (SizeAttr_Size x) _ -> return (make opMin $ fromList [x, innerSize])
            MSetAttr (SizeAttr_MaxSize x) _ -> return (make opMin $ fromList [x, innerSize])
            MSetAttr (SizeAttr_MinMaxSize _ x) _ -> return (make opMin $ fromList [x, innerSize])
            _ -> fail ("gDomainSizeOf.getMaxSize, mset not supported. attributes:" <+> pretty attrs)
    maxSize  <- getMaxSize
    maxOccur <- getMaxOccur
    return (make opPow maxOccur maxSize)
gDomainSizeOf (DomainFunction _ (FunctionAttr sizeAttr _ _) innerFr innerTo) =
    gDomainSizeOf $ DomainRelation () (RelationAttr sizeAttr def) [innerFr, innerTo]
gDomainSizeOf (DomainRelation _ (RelationAttr sizeAttr binRelAttr) inners) | binRelAttr == def =
    gDomainSizeOf (DomainSet () (SetAttr sizeAttr) (DomainTuple inners))
gDomainSizeOf (DomainPartition _ a inner) =
    gDomainSizeOf $ DomainSet () (SetAttr (partsNum  a))
                  $ DomainSet () (SetAttr (partsSize a)) inner
gDomainSizeOf d = fail ("not implemented: gDomainSizeOf:" <+> pretty d)


instance ( ExpressionLike x
         , OperatorContainer x
         , Pretty x
         , Num x
         , Show x
         ) => DomainSizeOf (Range x) x where
    domainSizeOf RangeSingle{} = return 1
    domainSizeOf (RangeBounded l u) = return $ make opSum $ fromList [1, make opMinus u l]
    domainSizeOf r = fail ("domainSizeOf infinite range:" <+> pretty r)
    



-- Nothing means an infinite domain
domainSizeConstant :: MonadFail m => Domain r Constant -> m Int
domainSizeConstant DomainBool{} = return 2
domainSizeConstant (DomainInt rs) = domainSizeConstantRanges rs
domainSizeConstant DomainEnum{} = fail "domainSizeConstant: Unknown for given enum."
domainSizeConstant (DomainTuple ds) = product <$> mapM domainSizeConstant ds
domainSizeConstant (DomainMatrix index inner) = (^) <$> domainSizeConstant inner <*> domainSizeConstant index
domainSizeConstant (DomainSet _ (SetAttr attrs) inner) =
    case attrs of
        SizeAttr_None -> do
            innerSize <- domainSizeConstant inner
            return (2 ^ innerSize)
        SizeAttr_Size (ConstantInt size) -> do
            innerSize <- domainSizeConstant inner
            return (nchoosek (product . enumFromTo 1) innerSize size)
        SizeAttr_MaxSize (ConstantInt maxSize) -> do
            innerSize <- domainSizeConstant inner
            return $ sum [ nchoosek (product . enumFromTo 1) innerSize k | k <- [0 .. maxSize] ]
        SizeAttr_MinMaxSize (ConstantInt minSize) (ConstantInt maxSize) -> do
            innerSize <- domainSizeConstant inner
            return $ sum [ nchoosek (product . enumFromTo 1) innerSize k | k <- [minSize .. maxSize] ]
        _ -> fail "domainSizeConstant"
domainSizeConstant (DomainMSet      {}) = bug "not implemented: domainSizeConstant DomainMSet"
domainSizeConstant (DomainFunction  {}) = bug "not implemented: domainSizeConstant DomainFunction"
domainSizeConstant (DomainRelation  {}) = bug "not implemented: domainSizeConstant DomainRelation"
domainSizeConstant (DomainPartition {}) = bug "not implemented: domainSizeConstant DomainPartition"
domainSizeConstant _                    = bug "not implemented: domainSizeConstant"

domainSizeConstantRanges :: MonadFail m => [Range Constant] -> m Int
domainSizeConstantRanges = liftM length . valuesInIntDomain

nchoosek :: (Num a, Integral a) => (a -> a) -> a -> a -> a
nchoosek f n k = f n `div` (f k * f (n-k))

enumNameToInt :: [Name] -> Name -> Int
enumNameToInt nms nm = case elemIndex nm nms of
    Nothing -> bug $ vcat [ pretty nm <+> "is not a value of this enumerated type."
                          , "Values are:" <+> prettyList id "," (map pretty nms)
                          ]
    Just i  -> i + 1

enumIntToName :: [Name] -> Int -> Name
enumIntToName nms i = at nms (i-1)

