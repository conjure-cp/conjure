{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module Conjure.RefineParam ( refineSingleParam, refineParam ) where

-- conjure
import Conjure.UpDown
import Language.E.Imports
import Language.E.Definition


-- | Refining a single high level constant (with a given high level domain) to a low level constant (and the low level domain).
--   A tree of representations is taken as an argument. The tree contains representations for each level of nesting for the domain.
refineSingleParam
    :: MonadError UpDownError m
    =>    (Text, Domain Representation Constant, Constant)
    -> m [(Text, Domain Representation Constant, Constant)]
refineSingleParam (name, highDomain, highConstant) = do
    (lowDomainsGen, _, _, lowConstantsGen, _) <- upDown highDomain
    lowDomains <- lowDomainsGen
    lowConstants <- lowConstantsGen name highConstant
    return [ (n,d,c)
           | d <- lowDomains
           | (n,c) <- lowConstants
           ]


refineParam
    :: MonadError UpDownError m
    => Spec                         -- ^ Essence
    -> Maybe Spec                   -- ^ Essence Parameter
    -> Spec                         -- ^ Essence'
    -> m Spec                       -- ^ Essence' Parameter
refineParam = error "refineParam"

