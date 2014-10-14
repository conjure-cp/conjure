{-# LANGUAGE Rank2Types #-}

module Conjure.Representations.Internal
    ( Representation(..)
    , DomainX, DomainC
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty


type DomainX x = Domain HasRepresentation x
type DomainC = Domain HasRepresentation Constant

-- the rDown_, rDown, and rUp all work one level at a time.
-- the maybe for rDown_ and rDown is Nothing when representation doesn't change anything.
-- like for primitives.
data Representation m = Representation
    { rCheck :: forall x r . (Pretty x, ExpressionLike x)
             => (Domain r x -> [Domain HasRepresentation x])        -- other checkers for inner domains
             -> Domain r x                                          -- this domain
             -> [Domain HasRepresentation x]                        -- with all repr options
    , rDown_ :: forall x . (Pretty x, ExpressionLike x)
             => (Name, DomainX x)                     -> m (Maybe [(Name, DomainX x)])
    , rDown  :: (Name, DomainC, Constant)             -> m (Maybe [(Name, DomainC, Constant)])
    , rUp    :: [(Name, Constant)] -> (Name, DomainC) -> m (Name, Constant)
    }
    
