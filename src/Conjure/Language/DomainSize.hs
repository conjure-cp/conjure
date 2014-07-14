{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}

module Conjure.Language.DomainSize ( domainSizeConstant ) where

-- conjure
import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty


-- Nothing means an infinite domain
domainSizeConstant :: (Applicative m, MonadError Doc m) => Domain r Constant -> m Int
domainSizeConstant DomainBool = return 2
domainSizeConstant (DomainInt rs) = domainSizeConstantRanges rs
domainSizeConstant (DomainEnum _ rs) = domainSizeConstantRanges rs
domainSizeConstant (DomainTuple ds) = product <$> mapM domainSizeConstant ds
domainSizeConstant (DomainMatrix index inner) = (^) <$> domainSizeConstant inner <*> domainSizeConstant index
domainSizeConstant (DomainSet _ attrs inner) =
    case attrs of
        SetAttrNone -> do
            innerSize <- domainSizeConstant inner
            return (2 ^ innerSize)
        SetAttrSize (ConstantInt size) -> do
            innerSize <- domainSizeConstant inner
            return (nchoosek innerSize size)
        SetAttrMaxSize (ConstantInt maxSize) -> do
            innerSize <- domainSizeConstant inner
            return $ sum [ nchoosek innerSize k | k <- [0 .. maxSize] ]
        SetAttrMinMaxSize (ConstantInt minSize) (ConstantInt maxSize) -> do
            innerSize <- domainSizeConstant inner
            return $ sum [ nchoosek innerSize k | k <- [minSize .. maxSize] ]
        _ -> throwError "domainSizeConstant"
domainSizeConstant (DomainMSet      {}) = throwError "not implemented: domainSizeConstant"
domainSizeConstant (DomainFunction  {}) = throwError "not implemented: domainSizeConstant"
domainSizeConstant (DomainRelation  {}) = throwError "not implemented: domainSizeConstant"
domainSizeConstant (DomainPartition {}) = throwError "not implemented: domainSizeConstant"
domainSizeConstant _ = throwError "not implemented: domainSizeConstantRanges"

domainSizeConstantRanges :: MonadError Doc m => [Range Constant] -> m Int
domainSizeConstantRanges ranges =
    if isFinite
        then return (length allValues)
        else throwError $ "Infinite integer range:" <+> prettyList id "," ranges

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

