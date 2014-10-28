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
import Conjure.Language.Ops ( OperatorContainer(..) )
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
      )
    => Domain r x -> m x
gDomainSizeOf DomainBool = return (fromInt 2)
gDomainSizeOf (DomainInt []) = fail "gDomainSizeOf infinite integer domain"
gDomainSizeOf (DomainInt rs) = make opSum <$> mapM domainSizeOf rs
gDomainSizeOf (DomainEnum n Nothing) = fail ("gDomainSizeOf unknown enum domain" <+> pretty n)
gDomainSizeOf (DomainEnum _ (Just (vals, rs))) =
    let rs' = fmap (fmap (fromInt . enumNameToInt vals)) rs
    in  gDomainSizeOf (DomainInt rs' :: Domain r x)
gDomainSizeOf (DomainUnnamed _ x) = return x
gDomainSizeOf (DomainTuple []) = fail "gDomainSizeOf: nullary tuple"
gDomainSizeOf (DomainTuple xs) = make opProduct <$> mapM gDomainSizeOf xs
gDomainSizeOf (DomainMatrix index inner) = make opPow <$> gDomainSizeOf inner <*> gDomainSizeOf index
gDomainSizeOf d = fail ("not implemented: gDomainSizeOf:" <+> pretty d)
-- gDomainSizeOf (DomainSet       r (SizeAttr x) (Domain r x))
-- gDomainSizeOf (DomainMSet      r (DomainAttributes x) (Domain r x))
-- gDomainSizeOf (DomainFunction  r (DomainAttributes x) (Domain r x) (Domain r x))
-- gDomainSizeOf (DomainRelation  r (DomainAttributes x) [Domain r x])
-- gDomainSizeOf (DomainPartition r (DomainAttributes x) (Domain r x))
-- gDomainSizeOf (DomainOp Name [Domain r x])
-- gDomainSizeOf (DomainHack x)


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
        SizeAttrNone -> do
            innerSize <- domainSizeConstant inner
            return (2 ^ innerSize)
        SizeAttrSize (ConstantInt size) -> do
            innerSize <- domainSizeConstant inner
            return (nchoosek innerSize size)
        SizeAttrMaxSize (ConstantInt maxSize) -> do
            innerSize <- domainSizeConstant inner
            return $ sum [ nchoosek innerSize k | k <- [0 .. maxSize] ]
        SizeAttrMinMaxSize (ConstantInt minSize) (ConstantInt maxSize) -> do
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

valuesInIntDomain :: MonadFail m => [Range Constant] -> m [Int]
valuesInIntDomain ranges =
    if isFinite
        then return allValues
        else fail $ "Expected finite integer ranges, but got:" <+> prettyList id "," ranges

    where

        allRanges :: [Maybe [Int]]
        allRanges =
            [ vals
            | r <- ranges
            , let vals = case r of
                    RangeSingle (ConstantInt x) -> return [x]
                    RangeBounded (ConstantInt l) (ConstantInt u) -> return [l..u]
                    _ -> Nothing
            ]

        isFinite :: Bool
        isFinite = Nothing `notElem` allRanges

        allValues :: [Int]
        allValues = nub $ concat $ catMaybes allRanges

nchoosek :: Int -> Int -> Int
nchoosek n k = product [1..n] `div` (product [1..k] * product [1..n-k])

enumNameToInt :: [Name] -> Name -> Int
enumNameToInt nms nm = case elemIndex nm nms of
    Nothing -> bug $ vcat [ pretty nm <+> "is not a value of this enumerated type."
                          , "Values are:" <+> prettyList id "," (map pretty nms)
                          ]
    Just i  -> i + 1

enumIntToName :: [Name] -> Int -> Name
enumIntToName nms i = at nms (i-1)

