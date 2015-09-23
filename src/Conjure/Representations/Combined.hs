{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Combined
    ( downD, downC, up
    , downD1, downC1, up1
    , downToX1
    , reprOptions, getStructurals
    , reprsStandardOrderNoLevels, reprsStandardOrder, reprsSparseOrder
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.Instantiate
import Conjure.Process.Enumerate ( EnumerateDomain )

import Conjure.Representations.Internal
import Conjure.Representations.Primitive
import Conjure.Representations.Tuple
import Conjure.Representations.Matrix
import Conjure.Representations.Record
import Conjure.Representations.Variant
import Conjure.Representations.Set.Occurrence
import Conjure.Representations.Set.Explicit
import Conjure.Representations.Set.ExplicitVarSizeWithMarker
import Conjure.Representations.Set.ExplicitVarSizeWithFlags
import Conjure.Representations.MSet.ExplicitVarSizeWithFlags
import Conjure.Representations.Function.Function1D
import Conjure.Representations.Function.Function1DPartial
import Conjure.Representations.Function.FunctionND
import Conjure.Representations.Function.FunctionNDPartial
import Conjure.Representations.Function.FunctionAsRelation
import Conjure.Representations.Sequence.ExplicitBounded
import Conjure.Representations.Relation.RelationAsMatrix
import Conjure.Representations.Relation.RelationAsSet
import Conjure.Representations.Partition.Occurrence
import Conjure.Representations.Partition.PartitionAsSet


-- | Refine (down) a domain, outputting refinement expressions (X) one level (1).
--   The domain is allowed to be at the class level.
downToX1
    :: (MonadFail m, NameGen m)
    => FindOrGiven
    -> Name
    -> DomainX Expression
    -> m [Expression]
downToX1 forg name domain = rDownToX (dispatch domain) forg name domain

-- | Refine (down) a domain (D), one level (1).
--   The domain is allowed to be at the class level.
downD1
    :: (MonadFail m, NameGen m)
    =>           (Name, DomainX Expression)
    -> m (Maybe [(Name, DomainX Expression)])
downD1 (name, domain) = rDownD (dispatch domain) (name, domain)

-- | Refine (down) a domain, together with a constant (C), one level (1).
--   The domain has to be fully instantiated.
downC1
    :: (MonadFail m, NameGen m)
    =>           (Name, DomainC, Constant)
    -> m (Maybe [(Name, DomainC, Constant)])
downC1 (name, domain, constant) = rDownC (dispatch domain) (name, domain, constant)


-- | Translate a bunch of low level constants up, one level.
--   The high level domain (i.e. the target domain) has to be given.
--   The domain has to be fully instantiated.
up1
    :: (MonadFail m, NameGen m)
    =>   (Name, DomainC)
    ->  [(Name, Constant)]
    -> m (Name, Constant)
up1 (name, domain) ctxt = rUp (dispatch domain) ctxt (name, domain)


-- | Refine (down) a domain (D), all the way.
--   The domain is allowed to be at the class level.
downD
    :: (MonadFail m, NameGen m)
    =>    (Name, DomainX Expression)
    -> m [(Name, DomainX Expression)]
downD inp@(_, domain) = do
    mout <- rDownD (dispatch domain) inp
    case mout of
        Nothing -> return [inp]
        Just outs -> fmap concat $ mapM downD outs

-- | Refine (down) a domain, together with a constant (C), all the way.
--   The domain has to be fully instantiated.
downC
    :: (MonadFail m, NameGen m)
    =>    (Name, DomainC, Constant)
    -> m [(Name, DomainC, Constant)]
downC inp0 = do
    let inp1 = case inp0 of (nm, dom, TypedConstant con _) -> (nm, dom, con)
                            _                              -> inp0
    mout <- downC1 inp1
    case mout of
        Nothing -> return [inp0]
        Just outs -> fmap concat $ mapM downC outs

-- | Translate a bunch of low level constants up, all the way.
--   The high level domain (i.e. the target domain) has to be given.
--   The domain has to be fully instantiated.
up
    :: (MonadFail m, NameGen m, EnumerateDomain m)
    =>  [(Name, Constant)]
    ->   (Name, DomainC)
    -> m (Name, Constant)
up ctxt (name, highDomain) = do
    toDescend'
        -- :: Maybe [(Name, DomainX x)]
        <- downD1 (name, fmap Constant highDomain)
    case toDescend' of
        Nothing ->
            case lookup name ctxt of
                Nothing -> fail $ vcat
                    $ ("No value for:" <+> pretty name)
                    : "Bindings in context:"
                    : prettyContext ctxt
                Just val -> return (name, val)
        Just toDescend -> do
            midConstants
                 :: [(Name, Constant)]
                 <- sequence [ do d' <- instantiateDomain [] d
                                  up ctxt (n, d')
                             | (n, d) <- toDescend
                             ]
            up1 (name, highDomain) midConstants


-- | Combine all known representations into one.
--   Dispatch into the actual implementation of the representation depending on the provided domain.
dispatch :: (MonadFail m, NameGen m, Pretty x) => Domain HasRepresentation x -> Representation m
dispatch domain = do
    let nope = bug $ "No representation for the domain:" <+> pretty domain
    case domain of
        DomainBool{}    -> primitive
        DomainInt{}     -> primitive
        DomainTuple{}   -> tuple
        DomainRecord{}  -> record
        DomainVariant{} -> variant
        DomainMatrix{}  -> matrix downD1 downC1 up1
        DomainSet r _ _ -> case r of
            "Occurrence"                    -> setOccurrence
            "Explicit"                      -> setExplicit
            "ExplicitVarSizeWithMarker"     -> setExplicitVarSizeWithMarker
            "ExplicitVarSizeWithFlags"      -> setExplicitVarSizeWithFlags
            _ -> nope
        DomainMSet r _ _ -> case r of
            "ExplicitVarSizeWithFlags"      -> msetExplicitVarSizeWithFlags
            _ -> nope
        DomainFunction r _ _ _ -> case r of
            "Function1D"                    -> function1D
            "Function1DPartial"             -> function1DPartial
            "FunctionND"                    -> functionND
            "FunctionNDPartial"             -> functionNDPartial
            "FunctionAsRelation"            -> functionAsRelation dispatch
            _ -> nope
        DomainSequence r _ _ -> case r of
            "ExplicitBounded"               -> sequenceExplicitBounded
            _ -> nope
        DomainRelation r _ _ -> case r of
            "RelationAsMatrix"              -> relationAsMatrix
            "RelationAsSet"                 -> relationAsSet dispatch
            _ -> nope
        DomainPartition r _ _ -> case r of
            "PartitionAsSet"                -> partitionAsSet dispatch
            "Occurrence"                    -> partitionOccurrence
            _ -> nope
        _ -> nope

type AllRepresentations = [[Representation (Either Doc)]]

-- | No levels!
reprsStandardOrderNoLevels :: AllRepresentations
reprsStandardOrderNoLevels = [concat reprsStandardOrder]

-- | A list of all representations.
--   As a crude measure, implementing levels here.
--   We shouldn't have levels between representations in the long run.
reprsStandardOrder :: AllRepresentations
reprsStandardOrder =
    [ [ primitive, tuple, record, variant, matrix downD1 downC1 up1
      , setOccurrence, setExplicit, setExplicitVarSizeWithMarker, setExplicitVarSizeWithFlags
      , msetExplicitVarSizeWithFlags
      , function1D, function1DPartial, functionND, functionNDPartial
      , sequenceExplicitBounded
      , relationAsMatrix
      -- , partitionOccurrence
      ]
    , [ functionAsRelation dispatch
      , relationAsSet dispatch
      , partitionAsSet dispatch
      ]
    ]

reprsSparseOrder :: AllRepresentations
reprsSparseOrder = map return
    [ primitive, tuple, record, variant, matrix downD1 downC1 up1

    , setExplicit, setExplicitVarSizeWithMarker
    , setOccurrence, setExplicitVarSizeWithFlags              -- redundant

    , msetExplicitVarSizeWithFlags

    , function1D, functionND
    , functionAsRelation dispatch
    , function1DPartial, functionNDPartial                    -- redundant

    , sequenceExplicitBounded

    , relationAsSet dispatch
    , relationAsMatrix

    , partitionAsSet dispatch
    ]


-- | For a domain, produce a list of domains with different representation options.
--   This function should never return an empty list.
reprOptions :: (Pretty r, Pretty x, ExpressionLike x) => AllRepresentations -> Domain r x -> [Domain HasRepresentation x]
reprOptions reprs domain = go reprs
    where
        go [] = []
        go (reprsThisLevel:reprsNextLevels) =
            let matchesOnThisLevel = concat [ rCheck r (reprOptions reprs) domain | r <- reprsThisLevel ]
            in  if null matchesOnThisLevel
                    then go reprsNextLevels
                    else matchesOnThisLevel


-- | For a domain, returns the structural constraints.
--   Makes recursive calls to generate the complete structural constraints.
--   Takes in a function to refine inner guys.
getStructurals
    :: (MonadFail m, NameGen m)
    => (Expression -> m [Expression])
    -> DomainX Expression
    -> m (Expression -> m [Expression])
getStructurals downX1 domain = rStructural (dispatch domain) (getStructurals downX1) downX1 domain

