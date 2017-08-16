{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Representations.PartitionSequence.PartitionSequenceAsSet ( partitionSequenceAsSet ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Constant
import Conjure.Language.Domain
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Language.Expression.DomainSizeOf ( domainSizeOf )
import Conjure.Representations.Internal


partitionSequenceAsSet
    :: forall m . (MonadFail m, NameGen m)
    => (forall x . DispatchFunction m x)
    -> (forall r x . ReprOptionsFunction m r x)
    -> Bool
    -> Representation m
partitionSequenceAsSet dispatch reprOptions useLevels = Representation chck downD structuralCons downC up

    where

        chck :: TypeOf_ReprCheck m
        chck _ dom1@(DomainPartitionSequence _ attrs _) = do
            -- this is a "lookahead"
            -- do the horizontal representation move: go from "partitionSequence of T" to "set of sequence of T"
            -- do representation selection on the set
            -- lookup the chosen representations and store them inside PartitionSequence_AsSet
            dom2 <- outDomain_ dom1
            dom3 <- reprOptions dom2
            return [ DomainPartitionSequence (PartitionSequence_AsSet r1 r2) attrs innerDomain
                   | DomainSet r1 _ (DomainSequence r2 _ innerDomain) <- dom3
                   -- special hack: do not use Set_ExplicitVarSizeWithFlags when --representation-levels=yes
                   , if useLevels
                       then r1 /= Set_ExplicitVarSizeWithFlags && r2 /= Set_ExplicitVarSizeWithFlags
                       else True
                   ]
        chck _ _ = return []

        outName :: Domain HasRepresentation x -> Name -> Name
        outName = mkOutName Nothing

        outDomain_ :: Pretty x => Domain () x -> m (Domain () x)
        outDomain_ (DomainPartitionSequence () PartitionAttr{..} innerDomain) =
            return (DomainSet () (SetAttr partsNum) (DomainSequence () (SequenceAttr partsSize JectivityAttr_None) innerDomain))
        outDomain_ domain = na $ vcat [ "{outDomain_} PartitionSequenceAsSet"
                                      , "domain:" <+> pretty domain
                                      ]

        outDomain :: Pretty x => Domain HasRepresentation x -> m (Domain HasRepresentation x)
        outDomain (DomainPartitionSequence (PartitionSequence_AsSet repr1 repr2) PartitionAttr{..} innerDomain) =
            return (DomainSet repr1 (SetAttr partsNum) (DomainSequence repr2 (SequenceAttr partsSize JectivityAttr_None) innerDomain))
        outDomain domain = na $ vcat [ "{outDomain} PartitionSequenceAsSet"
                                     , "domain:" <+> pretty domain
                                     ]

        downD :: TypeOf_DownD m
        downD (name, inDom) = do
            outDom <- outDomain inDom
            return $ Just [ ( outName inDom name , outDom ) ]

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 inDom@(DomainPartitionSequence _ attrs innerDomain) = return $ \ inpRel -> do
            refs <- downX1 inpRel
            let

                fixedPartSize =
                    case attrs of
                        PartitionAttr _ SizeAttr_Size{} _ -> True
                        _                                 -> False

                exactlyOnce rel = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return $ -- for list
                        [essence|
                            allDiff([ &j[2]
                                    | &iPat <- &rel
                                    , &jPat <- &i
                                    ])
                                |]

                regular rel | isRegular attrs && not fixedPartSize = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            and([ |&i| = |&j|
                                | &iPat <- &rel
                                , &jPat <- &rel
                                ])
                        |]
                regular _ = return []

                partsAren'tEmpty rel = do
                    (iPat, i) <- quantifiedVar
                    return $ return [essence| and([ |&i| >= 1 | &iPat <- &rel ]) |]

                sumOfParts rel = do
                    case domainSizeOf innerDomain of
                        Left _err -> return []
                        Right n   -> do
                            (iPat, i) <- quantifiedVar
                            return $ return [essence| &n = sum([ |&i| | &iPat <- &rel ]) |]

            case refs of
                [rel] -> do
                    outDom                 <- outDomain inDom
                    innerStructuralConsGen <- f outDom
                    concat <$> sequence
                        [ exactlyOnce rel
                        , regular rel
                        , partsAren'tEmpty rel
                        , innerStructuralConsGen rel
                        , sumOfParts rel
                        ]
                _ -> na $ vcat [ "{structuralCons} PartitionSequenceAsSet"
                               , pretty inDom
                               ]
        structuralCons _ _ domain = na $ vcat [ "{structuralCons} PartitionSequenceAsSet"
                                              , "domain:" <+> pretty domain
                                              ]

        downC :: TypeOf_DownC m
        downC ( name
              , inDom
              , ConstantAbstract (AbsLitPartitionSequence vals)
              ) = do
            outDom <- outDomain inDom
            rDownC
                (dispatch outDom)
                ( outName inDom name
                , outDom
                , ConstantAbstract $ AbsLitSet $ map (ConstantAbstract . AbsLitSequence) vals
                )
        downC (name, domain, constant) = na $ vcat [ "{downC} PartitionSequenceAsSet"
                                                   , "name    :" <+> pretty name
                                                   , "domain  :" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainPartitionSequence PartitionSequence_AsSet{} _ _)) =
            case lookup (outName domain name) ctxt of
                Nothing -> fail $ vcat $
                    [ "(in PartitionSequenceAsSet up)"
                    , "No value for:" <+> pretty (outName domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just (viewConstantSet -> Just sets) -> do
                    let seqOut (viewConstantSequence -> Just xs) = return xs
                        seqOut c = fail $ "Expecting a sequence, but got:" <++> pretty c
                    vals <- mapM seqOut sets
                    return (name, ConstantAbstract (AbsLitPartitionSequence vals))
                Just (ConstantUndefined msg ty) ->        -- undefined propagates
                    return (name, ConstantUndefined ("PartitionSequenceAsSet " `mappend` msg) ty)
                Just constant -> fail $ vcat $
                    [ "Incompatible value for:" <+> pretty (outName domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    , "Expected a set value, but got:" <++> pretty constant
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ (name, domain) = na $ vcat [ "{up} PartitionSequenceAsSet"
                                        , "name:" <+> pretty name
                                        , "domain:" <+> pretty domain
                                        ]
