{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.IntroduceRegions
    ( introduceRegions
    , removeRegions
    ) where

import Language.E

import Data.Text ( pack )


-- identifiers do not have regions attached to them at this point.
-- attach a region to each an every identifier.
-- applyRepr should respect these regions: identifiers with the same region
-- should be represented in the same way.
introduceRegions :: MonadConjure m => Bool -> Spec -> m Spec
introduceRegions useChannelling spec = withBindingScope' $
    let
        regions = if useChannelling
                    then [ "region" `mappend` pack (show i) | i <- [ 1::Int .. ] ]
                    else repeat "regionS"
    in
        evalStateT (bottomUpSpecExcept skip op spec) regions
    where
        skip [xMatch| _ := topLevel.declaration.find  |] = True
        skip [xMatch| _ := topLevel.declaration.given |] = True
        skip [xMatch| _ := topLevel.letting           |] = True
        skip _ = False

        op p@[xMatch| [Prim (S s)] := reference |] = case identifierSplit s of
            (_, Just _, _) -> return p
            (base, Nothing, repr) -> do
                mx <- lift $ runMaybeT $ lookupReference s
                case mx of
                    Just [xMatch| [D d] := topLevel.declaration.find .domain |]
                        | domainNeedsRepresentation d -> addRegion base repr
                    Just [xMatch| [D d] := topLevel.declaration.given.domain |]
                        | domainNeedsRepresentation d -> addRegion base repr
                    _ -> return p
        op p = return p

        -- get the next region from state, and add it to the given identifier
        addRegion base repr = do
            r <- nextRegion
            let s' = identifierConstruct base (Just r) repr
            return [xMake| reference := [Prim (S s')] |]

        nextRegion = do
            (i:is) <- get
            put is
            return i

removeRegions :: MonadConjure m => Spec -> m Spec
removeRegions = withBindingScope' . bottomUpSpec' f
    where
        f [xMatch| [Prim (S s)] := reference |] = do
            let (base, _, _) = identifierSplit s
            return [xMake| reference := [Prim (S base)] |]
        f p = return p

