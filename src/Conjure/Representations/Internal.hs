{-# LANGUAGE Rank2Types #-}

module Conjure.Representations.Internal
    ( Representation(..)
    , DomainX, DomainC
    , rDownToX
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty


type DomainX x = Domain HasRepresentation x
type DomainC = Domain HasRepresentation Constant

-- | This data type represents a representation selection rule.
-- The 3 functions -- rDownD, rDownC, and rUp -- all work one level at a time.
-- The maybe for rDownD and rDownC is Nothing when representation doesn't change anything.
-- Like for primitives.
-- * rDownD is for refining a class level domain.
-- * rDownC is for refining constants, together with an instance level domain.
-- * rUp is for translating low level constants upwards
--   It takes in a instance level domain for the high level object.
-- * rCheck is for calculating all representation options.
--   It take a function to be used as a "checker" for inner domains, if any.
data Representation m = Representation
    { rCheck
        :: forall x r . (Pretty x, ExpressionLike x)
        => (Domain r x -> [DomainX x])                  -- other checkers for inner domains
        -> Domain r x                                   -- this domain
        -> [DomainX x]                                  -- with all repr options
    , rDownD
        :: (Name, DomainX Expression)
        -> m (Maybe [(Name, DomainX Expression)])
    , rStructural
        :: (DomainX Expression -> m ([Name] -> [Expression] -> m [Expression]))
                                                        -- other structural constraints for inner domains
        -> (Expression -> m [Expression])               -- general downX1
        -> DomainX Expression                           -- this domain
        -> m (     [Name]                               -- a source of fresh names
              ->   [Expression]                         -- refined variables
              -> m [Expression]                         -- structural constraints
             )
    , rDownC
        :: (Name, DomainC, Constant)                    -- the input name, domain and constant
        -> m (Maybe [(Name, DomainC, Constant)])        -- the outputs names, domains, and constants
    , rUp
        :: [(Name, Constant)]                           -- all known constants, representing a solution at the low level
        -> (Name, DomainC)                              -- the name and domain we are working on
        -> m (Name, Constant)                           -- the output constant, at the high level
    }


rDownToX
    :: Monad m
    => Representation m                                 -- for a given representation
    -> FindOrGiven                                      -- and a declaration: forg
    -> Name                                             --                  : name
    -> Domain HasRepresentation Expression              --                  : domain
    -> m [Expression]                                   -- expressions referring to the referring
rDownToX repr forg name domain = do
    mpairs <- rDownD repr (name, domain)
    return $ case mpairs of
        Nothing    -> []
        Just pairs -> [ Reference n (Just (DeclHasRepr forg n d))
                      | (n,d) <- pairs
                      ]
