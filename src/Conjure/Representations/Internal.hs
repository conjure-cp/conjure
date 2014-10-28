{-# LANGUAGE Rank2Types #-}

module Conjure.Representations.Internal
    ( Representation(..)
    , DomainX, DomainC
    , rDownX
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
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
    { rCheck      :: forall x r . (Pretty x, ExpressionLike x)
                  => (Domain r x -> [DomainX x])                       -- other checkers for inner domains
                  -> Domain r x                                        -- this domain
                  -> [DomainX x]                                       -- with all repr options
    , rDownD      :: (Name, DomainX Expression)            -> m (Maybe [(Name, DomainX Expression)])
    , rStructural :: (Name, DomainX Expression)            -> m (Maybe (  [Name]          -- a source of fresh names
                                                                       -> [Expression]    -- structural constraints
                                                                       ))
    , rDownC      :: (Name, DomainC, Constant)             -> m (Maybe [(Name, DomainC, Constant)])
    , rUp         :: [(Name, Constant)] -> (Name, DomainC) -> m (Name, Constant)
    }


rDownX
    :: Monad m
    => Representation m
    -> Name
    -> Domain HasRepresentation Expression
    -> m [Expression]
rDownX repr name domain = do
    mpairs <- rDownD repr (name, domain)
    case mpairs of
        Nothing -> bug ("rDownX, rDownD doesn't work:" <++> pretty name)
        (Just pairs) -> return [ Reference n (Just (DeclHasRepr Find n d))
                               | (n,d) <- pairs
                               ]

