{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module Conjure.RefineParam ( refineSingleParam, refineParam ) where

-- conjure
import Conjure.UpDown
import Language.E.Imports
import Language.E.Definition

-- base
import Data.List ( transpose )


-- | Refining a single high level constant (with a given high level domain) to a low level constant (and the low level domain).
--   A tree of representations is taken as an argument. The tree contains representations for each level of nesting for the domain.
refineSingleParam
    :: MonadError UpDownError m
    =>    (Text, Domain Representation Constant, Constant)
    -> m [(Text, Domain Representation Constant, Constant)]

refineSingleParam pack@(_, DomainBool{}, _) = return [pack]
refineSingleParam pack@(_, DomainInt{}, _) = return [pack]

-- if the domain is a matrix, we treat it as a container.
-- - the matrix domain itself doesn't have a representation.
-- - the index domain will stay unchanged.
-- - the inner domain will be handled with a call to `refineSingleParam`, and its results will be "lifted".
refineSingleParam (name, DomainMatrix index highDomain, ConstantMatrix _ highConstants@(_:_)) = do
    mids <- sequence [ refineSingleParam (name, highDomain, c)
                     | c <- highConstants
                     ]
    forM (transpose mids) $ \ mid ->
        case mid of
            ((midName, midDomain, _):_) -> do
                let midConstants = [ c | (_,_,c) <- mid ]
                return ( midName
                       , DomainMatrix index midDomain
                       , ConstantMatrix index midConstants
                       )
            [] -> error "refineSingleParam []"

-- the generic case.
-- - use `upDown` to refine the domain down one level.
-- - use a recursive call to `refineSingleParam` to handle the outputs.
refineSingleParam (name, highDomain, highConstant) = do
    (lowDomainsGen, lowNamesGen, _, lowConstantsGen, _) <- upDown highDomain
    let lowNames = map ($ name) lowNamesGen
    lowDomains <- lowDomainsGen
    lowConstants <- lowConstantsGen highConstant
    liftM concat $ sequence
        [ refineSingleParam (n,d,c)
        | n <- lowNames
        | d <- lowDomains
        | c <- lowConstants
        ]


refineParam
    :: MonadError UpDownError m
    => Spec                         -- ^ Essence
    -> Maybe Spec                   -- ^ Essence Parameter
    -> Spec                         -- ^ Essence'
    -> m Spec                       -- ^ Essence' Parameter
refineParam = error "refineParam"

