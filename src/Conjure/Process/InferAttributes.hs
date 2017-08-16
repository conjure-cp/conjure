{-# LANGUAGE QuasiQuotes #-}

-- | This is an extremely simplified version of type-strengthening
module Conjure.Process.InferAttributes ( inferAttributes ) where

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.Domain.AddAttributes ( mkMin )
import Conjure.Language.Expression.DomainSizeOf ( domainSizeOf )


inferAttributes :: MonadFail m => Model -> m Model
inferAttributes m = transformBiM inferAttributesD m

inferAttributesD :: MonadFail m => Domain () Expression -> m (Domain () Expression)
inferAttributesD (DomainPartition () (PartitionAttr partsNum1 partsSize1 isRegular1) innerDomain) = do
    -- there cannot be more parts than there are members
    let partsNum2 =
            case domainSizeOf innerDomain of
                Left _err -> partsNum1
                Right n  -> case partsNum1 of
                                SizeAttr_None -> SizeAttr_MaxSize n
                                SizeAttr_Size x -> SizeAttr_Size x
                                SizeAttr_MinSize x -> SizeAttr_MinMaxSize x n
                                SizeAttr_MaxSize x -> SizeAttr_MaxSize (mkMin x n)
                                SizeAttr_MinMaxSize x y -> SizeAttr_MinMaxSize x (mkMin y n)
    return (DomainPartition () (PartitionAttr partsNum2 partsSize1 isRegular1) innerDomain)
inferAttributesD d = return d

