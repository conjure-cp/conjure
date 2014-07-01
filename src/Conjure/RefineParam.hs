{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}

module Conjure.RefineParam ( refineSingleParam, refineParam ) where

-- conjure
import Conjure.UpDown
import Language.E.Imports
import Language.E.Definition

-- base
import Data.List ( transpose )

-- containers
import Data.Tree ( Tree(..) )


-- | Refining a single high level constant (with a given high level domain) to a low level constant (and the low level domain).
--   A tree of representations is taken as an argument. The tree contains representations for each level of nesting for the domain.
refineSingleParam
    :: MonadError UpDownError m
    => Tree Representation
    ->    (Text, Domain Representation Constant, Constant)
    -> m [(Text, Domain Representation Constant, Constant)]

-- if the domain is a matrix, we treat it as a container.
-- - the matrix domain itself must have `NoRepresentation`. the tree doesn't even contain an entry for it.
-- - the index domain will stay unchanged.
-- - the inner domain will be handled with a call to `refineSingleParam`, and its results will be "lifted".
refineSingleParam representation (name, DomainMatrix index highDomain, ConstantMatrix _ highConstants@(_:_)) = do
    mids <- sequence [ refineSingleParam representation (name, highDomain, c)
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
refineSingleParam (Node _representation representations) (name, highDomain, highConstant) = do
    (lowDomainsGen, lowNamesGen, _, lowConstantsGen, _) <- upDown highDomain
    let lowNames = map ($ name) lowNamesGen
    lowDomains <- lowDomainsGen
    lowConstants <- lowConstantsGen highConstant
    if null representations
        then 
            return
                [ (n,d,c)
                | n <- lowNames
                | d <- lowDomains
                | c <- lowConstants
                ]
        else
            liftM concat $ sequence
                [ refineSingleParam r (n,d,c)
                | r <- representations
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

