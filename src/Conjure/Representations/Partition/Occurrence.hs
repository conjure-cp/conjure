{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Partition.Occurrence ( partitionOccurrence ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Constant ( normaliseConstant )
import Conjure.Language.Domain
import Conjure.Language.DomainSize
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainValues, toIntDomain )


partitionOccurrence :: MonadFail m => Representation m
partitionOccurrence = Representation chck downD structuralCons downC up

    where

        chck f (DomainPartition _ attrs innerDomain)
            | domainCanIndexMatrix innerDomain
            = DomainPartition "Occurrence" attrs <$> f innerDomain
        chck _ _ = []

        nameFlags    name = mconcat [name, "_", "Occurrence", "_", "Flags"]
        nameParts    name = mconcat [name, "_", "Occurrence", "_", "Parts"]
        nameNumParts name = mconcat [name, "_", "Occurrence", "_", "NumParts"]

        downD (name, DomainPartition "Occurrence" (PartitionAttr{..}) innerDomain')
            | domainCanIndexMatrix innerDomain' = do
            innerDomain <- toIntDomain innerDomain'
            maxNbParts  <- domainSizeOf innerDomain
            return $ Just
                [ ( nameFlags name
                  , DomainMatrix
                      (forgetRepr "Representation.PartitionOccurrence" innerDomain)
                      DomainBool
                  )
                , ( nameParts name
                  , DomainMatrix
                      (forgetRepr "Representation.PartitionOccurrence" innerDomain)
                      (DomainInt [RangeBounded 1 maxNbParts])
                  )
                , ( nameNumParts name
                  , DomainInt [RangeBounded 0 maxNbParts]
                  )
                ]
        downD _ = na "{downD} Occurrence"

        structuralCons _ downX1 (DomainPartition _ attrs innerDomain')
                | domainCanIndexMatrix innerDomain'
                = do
            innerDomain <- toIntDomain innerDomain'
            let

                nbParticipants fresh flags =
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence|
                            sum &iPat : &innerDomain . toInt(&flags[&i])
                        |]

                numPartsCons fresh flags parts numPartsVar =
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  return -- for list
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

                noGaps fresh flags parts numPartsVar =
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                        (pPat, p) = quantifiedVar (fresh `at` 1)
                    in  return -- for list
                        [essence|
                            and([ or([ &flags[&i] /\ &parts[&i] = &p    $ the part has to contain elems
                                     | &iPat : &innerDomain
                                     ])
                                | &pPat : &innerDomain                  $ for all part nums
                                , &p <= &numPartsVar                    $ that are active
                                ])
                        |]

                dontCares fresh flag parts =
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  return -- for list
                        [essence|
                            and([ !&flag[&i] -> dontCare(&parts[&i])
                                | &iPat : &innerDomain
                                ])
                        |]

                regular fresh rel | isRegular attrs =
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)
                    in  return -- for list
                        [essence|
                            and([ |&i| = |&j|
                                | &iPat <- parts(&rel)
                                , &jPat <- parts(&rel)
                                ])
                        |]
                regular _ _ = []

                complete fresh flags | isComplete attrs =
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  return -- for list
                        [essence|
                            and([ &flags[&i]
                                | &iPat : &innerDomain
                                ])
                        |]
                complete _ _ = []

                -- participantsCardinality rel =
                --     let (iPat, i) = quantifiedVar (fresh `at` 0)
                --     in  [essence| sum([ |&i| | &iPat <- &rel ]) |]

            return $ \ fresh inpPartition -> do
                [flags, parts, numPartsVar] <- downX1 inpPartition
                return $ concat
                    [ mkSizeCons (participantsSize attrs) (nbParticipants fresh flags)
                    , mkSizeCons (partsNum         attrs) numPartsVar
                    -- , mkSizeCons  (participantsCardinality rel)
                    , numPartsCons fresh flags parts numPartsVar
                    , regular      fresh inpPartition
                    , complete     fresh flags
                    , noGaps       fresh flags parts numPartsVar
                    , dontCares    fresh flags parts
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
        downC (name, domain, constant) = na $ vcat [ "{downC} Occurrence"
                                                   , "name:" <+> pretty name
                                                   , "domain:" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]


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
