{-# LANGUAGE MultiParamTypeClasses, FlexibleInstances, FlexibleContexts #-}
{-# LANGUAGE ScopedTypeVariables #-}

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
    domainSizeOf (DomainEnum n Nothing) = return $
        let n' = n `mappend` "_EnumSize"
        in  Reference n' (Just (DeclHasRepr Given n' (DomainInt [])))
    domainSizeOf d = gDomainSizeOf d


gDomainSizeOf
    :: forall m x r
    . ( MonadFail m
      , ExpressionLike x
      , OperatorContainer x
      , Pretty x
      , Pretty r
      , Integral x
      )
    => Domain r x -> m x
gDomainSizeOf DomainBool = return (fromInt 2)
gDomainSizeOf (DomainInt [] ) = fail "gDomainSizeOf infinite integer domain"
gDomainSizeOf (DomainInt [r]) = domainSizeOf r
gDomainSizeOf (DomainInt rs ) = make opSum <$> mapM domainSizeOf rs
gDomainSizeOf (DomainUnnamed _ x) = return x
gDomainSizeOf (DomainTuple []) = fail "gDomainSizeOf: nullary tuple"
gDomainSizeOf (DomainTuple xs) = make opProduct <$> mapM gDomainSizeOf xs
gDomainSizeOf (DomainMatrix index inner) = make opPow <$> gDomainSizeOf inner <*> gDomainSizeOf index
gDomainSizeOf d@(DomainSet _ (SetAttr sizeAttr) inner) = do
    innerSize <- gDomainSizeOf inner
    case sizeAttr of
        SizeAttr_None      -> return (make opPow (fromInt 2) innerSize)
        SizeAttr_Size size -> return (nchoosek innerSize size)
        _ -> fail ("gDomainSizeOf:" <+> pretty d)
gDomainSizeOf (DomainFunction _ (FunctionAttr _ PartialityAttr_Total _) innerFr innerTo) = do
    innerFrSize <- gDomainSizeOf innerFr
    innerToSize <- gDomainSizeOf innerTo
    return (nchoosek innerToSize innerFrSize)
gDomainSizeOf d = fail ("not implemented: gDomainSizeOf:" <+> pretty d)


instance ( ExpressionLike x
         , OperatorContainer x
         , Pretty x
         , Show x
         ) => DomainSizeOf (Range x) x where
    domainSizeOf RangeSingle{} = return (fromInt 1)
    domainSizeOf (RangeBounded l u) = return (make opPlus (fromInt 1) (make opMinus u l))
    domainSizeOf r = fail ("domainSizeOf infinite range:" <+> pretty r)
    



-- Nothing means an infinite domain
domainSizeConstant :: MonadFail m => Domain r Constant -> m Int
domainSizeConstant DomainBool = return 2
domainSizeConstant (DomainInt rs) = domainSizeConstantRanges rs
domainSizeConstant (DomainEnum _ _) = fail "domainSizeConstant: Unknown for given enum."
domainSizeConstant (DomainTuple ds) = product <$> mapM domainSizeConstant ds
domainSizeConstant (DomainMatrix index inner) = (^) <$> domainSizeConstant inner <*> domainSizeConstant index
domainSizeConstant (DomainSet _ (SetAttr attrs) inner) =
    case attrs of
        SizeAttr_None -> do
            innerSize <- domainSizeConstant inner
            return (2 ^ innerSize)
        SizeAttr_Size (ConstantInt size) -> do
            innerSize <- domainSizeConstant inner
            return (nchoosek innerSize size)
        SizeAttr_MaxSize (ConstantInt maxSize) -> do
            innerSize <- domainSizeConstant inner
            return $ sum [ nchoosek innerSize k | k <- [0 .. maxSize] ]
        SizeAttr_MinMaxSize (ConstantInt minSize) (ConstantInt maxSize) -> do
            innerSize <- domainSizeConstant inner
            return $ sum [ nchoosek innerSize k | k <- [minSize .. maxSize] ]
        _ -> fail "domainSizeConstant"
domainSizeConstant (DomainMSet      {}) = bug "not implemented: domainSizeConstant DomainMSet"
domainSizeConstant (DomainFunction  {}) = bug "not implemented: domainSizeConstant DomainFunction"
domainSizeConstant (DomainRelation  {}) = bug "not implemented: domainSizeConstant DomainRelation"
domainSizeConstant (DomainPartition {}) = bug "not implemented: domainSizeConstant DomainPartition"
domainSizeConstant _                    = bug "not implemented: domainSizeConstant"

domainSizeConstantRanges :: MonadFail m => [Range Constant] -> m Int
domainSizeConstantRanges = liftM length . valuesInIntDomain

nchoosek :: Integral a => a -> a -> a
nchoosek n k = product [1..n] `div` (product [1..k] * product [1..n-k])

enumNameToInt :: [Name] -> Name -> Int
enumNameToInt nms nm = case elemIndex nm nms of
    Nothing -> bug $ vcat [ pretty nm <+> "is not a value of this enumerated type."
                          , "Values are:" <+> prettyList id "," (map pretty nms)
                          ]
    Just i  -> i + 1

enumIntToName :: [Name] -> Int -> Name
enumIntToName nms i = at nms (i-1)

