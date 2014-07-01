{-# LANGUAGE OverloadedStrings #-}

module Conjure.Language.DomainSize ( domainSizeConstant ) where

-- conjure
import Language.E.Imports
import Language.E.Definition


-- Nothing means an infinite domain
domainSizeConstant :: Domain r Constant -> Maybe Int
domainSizeConstant DomainBool = Just 2
domainSizeConstant (DomainInt rs) = domainSizeConstantRanges rs
domainSizeConstant (DomainEnum _ rs) = domainSizeConstantRanges rs
domainSizeConstant (DomainTuple ds) = product <$> mapM domainSizeConstant ds
domainSizeConstant (DomainMatrix index inner) = (^) <$> domainSizeConstant inner <*> domainSizeConstant index
domainSizeConstant (DomainSet _ (DomainAttributes attrs) inner) =
    case attrs of
        [DANameValue "size" (ConstantInt size)] -> do
            innerSize <- domainSizeConstant inner
            return (nchoosek innerSize size)
        [DANameValue "maxSize" (ConstantInt maxSize)] -> do
            innerSize <- domainSizeConstant inner
            return $ sum [ nchoosek innerSize k | k <- [0 .. maxSize] ]
        [DANameValue "minSize" (ConstantInt minSize), DANameValue "maxSize" (ConstantInt maxSize)] -> do
            innerSize <- domainSizeConstant inner
            return $ sum [ nchoosek innerSize k | k <- [minSize .. maxSize] ]
        [] -> do
            innerSize <- domainSizeConstant inner
            -- return $ sum [ nchoosek innerSize k | k <- [0 .. innerSize] ]
            return (2 ^ innerSize)
        _ -> error "domainSizeConstant"
domainSizeConstant (DomainMSet      {}) = error "not implemented: domainSizeConstant"
domainSizeConstant (DomainFunction  {}) = error "not implemented: domainSizeConstant"
domainSizeConstant (DomainRelation  {}) = error "not implemented: domainSizeConstant"
domainSizeConstant (DomainPartition {}) = error "not implemented: domainSizeConstant"
domainSizeConstant _ = error "not implemented: domainSizeConstantRanges"

domainSizeConstantRanges :: [Range Constant] -> Maybe Int
domainSizeConstantRanges ranges =
    if isFinite
        then Just (length allValues)
        else Nothing

    where

        allRanges :: [Maybe [Int]]
        allRanges =
            [ vals
            | r <- ranges
            , let vals = case r of
                    RangeSingle (ConstantInt x) -> Just [x]
                    RangeBounded (ConstantInt l) (ConstantInt u) -> Just [l..u]
                    _ -> Nothing
            ]

        isFinite :: Bool
        isFinite = Nothing `notElem` allRanges

        allValues :: [Int]
        allValues = nub $ concat $ catMaybes allRanges

nchoosek :: Int -> Int -> Int
nchoosek n k = product [1..n] `div` (product [1..k] * product [1..n-k])

