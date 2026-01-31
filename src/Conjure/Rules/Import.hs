module Conjure.Rules.Import
    ( module X
    , doDuplicatesMatter
    , referenceToComprehensionVar
    ) where

import Conjure.Prelude as X
import Conjure.Bug as X

import Conjure.Language as X

import Conjure.Language.CategoryOf as X ( categoryOf, Category(..) )
import Conjure.Language.RepresentationOf as X ( representationOf, hasRepresentation, sameRepresentation )

import Conjure.Compute.DomainOf as X ( domainOf, indexDomainsOf )
import Conjure.Compute.DomainUnion as X ( domainUnion, domainUnions )
import Conjure.Language.DomainSizeOf as X ( domainSizeOf )
import Conjure.Language.Instantiate as X ( entailed )

import Conjure.Rules.Definition as X ( Rule(..), namedRule, namedRuleZ, matchFirst )
import Conjure.Representations as X ( downX1 )

-- uniplate
import Data.Generics.Uniplate.Zipper as Zipper ( Zipper, up, hole )



-- keep going up, until finding a quantifier
-- when found, return whether this quantifier requires us to remove duplicates or not
-- if none exists, do not apply the rule.
-- (or maybe we should call bug right ahead, it can't be anything else.)
doDuplicatesMatter :: MonadFailDoc m => Zipper.Zipper a Expression -> m Bool
doDuplicatesMatter z0 =
    case Zipper.up z0 of
        Nothing -> na "doDuplicatesMatter 1"
        Just z -> do
            let h = Zipper.hole z
            case ( match opAnd h, match opOr h, match opSum h
                 , match opProduct h, match opMin h, match opMax h ) of
                (Just{}, _, _, _, _, _) -> return False -- and
                (_, Just{}, _, _, _, _) -> return False -- or
                (_, _, Just{}, _, _, _) -> return True  -- sum
                (_, _, _, Just{}, _, _) -> return True  -- product
                (_, _, _, _, Just{}, _) -> return False -- min
                (_, _, _, _, _, Just{}) -> return False -- max
                _                       -> na "doDuplicatesMatter 2"
                                        -- case Zipper.up z of
                                        --     Nothing -> na "doDuplicatesMatter"
                                        --     Just u  -> doDuplicatesMatter u

referenceToComprehensionVar :: Expression -> Bool
referenceToComprehensionVar (Reference _ (Just InComprehension{})) = True
referenceToComprehensionVar (Op (MkOpIndexing (OpIndexing m _))) = referenceToComprehensionVar m
referenceToComprehensionVar _ = False

