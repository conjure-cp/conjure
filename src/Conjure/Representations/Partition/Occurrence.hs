{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Partition.Occurrence ( partitionOccurrence ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Constant ( normaliseConstant )
import Conjure.Language.Domain
import Conjure.Language.DomainSizeOf
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainValues )


partitionOccurrence :: forall m . (MonadFail m, NameGen m) => Representation m
partitionOccurrence = Representation chck downD structuralCons downC up

    where

        chck :: TypeOf_ReprCheck
        chck f (DomainPartition _ attrs innerDomain)
            | domainCanIndexMatrix innerDomain
            = DomainPartition "Occurrence" attrs <$> f innerDomain
        chck _ _ = []

        nameFlags    name = mconcat [name, "_", "Occurrence", "_", "Flags"]
        nameParts    name = mconcat [name, "_", "Occurrence", "_", "Parts"]
        nameNumParts name = mconcat [name, "_", "Occurrence", "_", "NumParts"]

        downD :: TypeOf_DownD m
        downD (name, DomainPartition "Occurrence" (PartitionAttr{..}) innerDomain)
            | domainCanIndexMatrix innerDomain = do
            maxNbParts <- domainSizeOf innerDomain
            return $ Just
                [ ( nameFlags name
                  , DomainMatrix
                      (forgetRepr innerDomain)
                      DomainBool
                  )
                , ( nameParts name
                  , DomainMatrix
                      (forgetRepr innerDomain)
                      (DomainInt [RangeBounded 1 maxNbParts])
                  )
                , ( nameNumParts name
                  , DomainInt [RangeBounded 0 maxNbParts]
                  )
                ]
        downD _ = na "{downD} Occurrence"

        structuralCons :: TypeOf_Structural m
        structuralCons _ downX1 (DomainPartition _ attrs innerDomain)
                | domainCanIndexMatrix innerDomain
                = do
            let
                nbParticipants flags = do
                    (iPat, i) <- quantifiedVar
                    return [essence| sum &iPat : &innerDomain . toInt(&flags[&i]) |]

                numPartsCons flags parts numPartsVar = do
                    (iPat, i) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            &numPartsVar =
                            max(flatten(
                                [ [0]                               $ default value
                                , [ toInt(&flags) * &parts[&i]      $ the maximum of ints, where flag=true
                                  | &iPat : &innerDomain
                                  ]
                                ] ))
                        |]

                -- nbParticipants

                noGaps flags parts numPartsVar = do
                    (iPat, i) <- quantifiedVar
                    (pPat, p) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            and([ or([ &flags[&i] /\ &parts[&i] = &p    $ the part has to contain elems
                                     | &iPat : &innerDomain
                                     ])
                                | &pPat : &innerDomain                  $ for all part nums
                                , &p <= &numPartsVar                    $ that are active
                                ])
                        |]

                dontCares flag parts = do
                    (iPat, i) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            and([ !&flag[&i] -> dontCare(&parts[&i])
                                | &iPat : &innerDomain
                                ])
                        |]

                regular rel | isRegular attrs = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            and([ |&i| = |&j|
                                | &iPat <- parts(&rel)
                                , &jPat <- parts(&rel)
                                ])
                        |]
                regular _ = return []

                complete flags = do
                    (iPat, i) <- quantifiedVar
                    return $ return -- for list
                        [essence|
                            and([ &flags[&i]
                                | &iPat : &innerDomain
                                ])
                        |]

                -- participantsCardinality rel =
                --     let (iPat, i) <- quantifiedVar
                --     in  [essence| sum([ |&i| | &iPat <- &rel ]) |]

            return $ \ inpPartition -> do
                [flags, parts, numPartsVar] <- downX1 inpPartition
                concat <$> sequence
                    [ mkSizeCons (participantsSize attrs) <$> nbParticipants flags
                    , return $ mkSizeCons (partsNum         attrs) numPartsVar
                    -- , mkSizeCons  (participantsCardinality rel)
                    , numPartsCons flags parts numPartsVar
                    , regular      inpPartition
                    , complete     flags
                    , noGaps       flags parts numPartsVar
                    , dontCares    flags parts
                    ]
        structuralCons _ _ domain = na $ vcat [ "{structuralCons} Occurrence"
                                              , "domain:" <+> pretty domain
                                              ]

        -- downC ( name
        --       , inDom
        --       , ConstantAbstract (AbsLitPartition vals)
        --       ) = do
        --     outDom <- outDomain (name, inDom)
        --     rDownC
        --         (dispatch outDom)
        --         ( outName name
        --         , outDom
        --         , ConstantAbstract $ AbsLitSet $ map (ConstantAbstract . AbsLitSet) vals
        --         )
        -- TODO
        downC :: TypeOf_DownC m
        downC (name, domain, constant) = na $ vcat [ "{downC} Occurrence"
                                                   , "name:" <+> pretty name
                                                   , "domain:" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainPartition "Occurrence" _ innerDomain)) =
            case (lookup (nameFlags name) ctxt, lookup (nameParts name) ctxt) of
                ( Just (ConstantAbstract (AbsLitMatrix _ flagMatrix)) ,
                  Just (ConstantAbstract (AbsLitMatrix _ partMatrix)) ) -> do
                    elems <- domainValues innerDomain
                    vals  <- liftM catMaybes $ forM (zip3 flagMatrix elems partMatrix) $ \ (flag, el, part) -> 
                        case flag of
                            ConstantBool b -> return $ if b then Just (part, el) else Nothing
                            _ -> fail $ vcat [ "Expected a boolean, but got:" <+> pretty flag
                                             , "When working on:" <+> pretty name
                                             , "With domain:" <+> pretty domain
                                             ]
                    let parts = sortNub $ map fst vals
                    return ( name
                           , normaliseConstant $ ConstantAbstract $ AbsLitPartition
                                [ [ el | (p', el) <- vals, p == p' ]
                                | p <- parts
                                ]
                           )
                (Nothing, _) -> fail $ vcat $
                    [ "No value for:" <+> pretty (nameFlags name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> fail $ vcat $
                    [ "No value for:" <+> pretty (nameParts name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                _ -> fail $ vcat $
                    [ "Expected matrix literals for:" <+> pretty (nameFlags name)
                                            <+> "and" <+> pretty (nameParts name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ (name, domain) = na $ vcat [ "{up} Occurrence"
                                        , "name:" <+> pretty name
                                        , "domain:" <+> pretty domain
                                        ]
