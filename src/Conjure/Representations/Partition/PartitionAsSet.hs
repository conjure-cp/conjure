{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Partition.PartitionAsSet ( partitionAsSet ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal
import Conjure.Representations.Common


partitionAsSet
    :: forall m . MonadFail m
    => (forall x . Pretty x => Domain HasRepresentation x -> Representation m)
    -> Representation m
partitionAsSet dispatch = Representation chck downD structuralCons downC up

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainPartition _ attrs innerDomain) = DomainPartition "PartitionAsSet" attrs <$> f innerDomain
        chck _ _ = []

        outName name = mconcat [name, "_", "PartitionAsSet"]

        outDomain (DomainPartition "PartitionAsSet" (PartitionAttr{..}) innerDomain) = do
            let repr1 = case partsNum of
                        SizeAttr_Size{} -> "Explicit"
                        _               -> "ExplicitVarSizeWithMarker"
            let repr2 = case partsSize of
                        SizeAttr_Size{} -> "Explicit"
                        _               -> "ExplicitVarSizeWithMarker"
            return (DomainSet repr1 (SetAttr partsNum) (DomainSet repr2 (SetAttr partsSize) innerDomain))
        outDomain domain = na $ vcat [ "{outDomain} PartitionAsSet"
                                     , "domain:" <+> pretty domain
                                     ]

        downD :: TypeOf_DownD m
        downD (name, inDom) = do
            outDom <- outDomain inDom
            return $ Just [ ( outName name , outDom ) ]

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 inDom@(DomainPartition _ attrs innerDomain) = return $ \ fresh inpRel -> do
            refs <- downX1 inpRel
            let
                atMostOnce rel =
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)
                    in
                        if isComplete attrs
                            then
                                [essence|
                                    forAll &iPat : &innerDomain .
                                        1  = sum ([ 1
                                                  | &jPat <- &rel
                                                  , &i in &j
                                                  ])
                                |]
                            else
                                [essence|
                                    forAll &iPat : &innerDomain .
                                        1 >= sum ([ 1
                                                  | &jPat <- &rel
                                                  , &i in &j
                                                  ])
                                |]

                regular rel | isRegular attrs =
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)
                    in  return -- for list
                        [essence|
                            and([ |&i| = |&j|
                                | &iPat <- &rel
                                , &jPat <- &rel
                                ])
                        |]
                regular _ = []

                participantsCardinality rel =
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| sum([ |&i| | &iPat <- &rel ]) |]

                partsAren'tEmpty rel =
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| and([ |&i| >= 1 | &iPat <- &rel ]) |]

            case refs of
                [rel] -> do
                    outDom                 <- outDomain inDom
                    innerStructuralConsGen <- f outDom
                    cons                   <- innerStructuralConsGen fresh rel
                    return $ concat
                        [ [ atMostOnce rel ]
                        , mkSizeCons (participantsSize attrs) (participantsCardinality rel)
                        , regular rel
                        , [ partsAren'tEmpty rel ]
                        , cons
                        ]
                _ -> na $ vcat [ "{structuralCons} PartitionAsSet"
                               , pretty inDom
                               ]
        structuralCons _ _ domain = na $ vcat [ "{structuralCons} PartitionAsSet"
                                              , "domain:" <+> pretty domain
                                              ]

        downC :: TypeOf_DownC m
        downC ( name
              , inDom
              , ConstantAbstract (AbsLitPartition vals)
              ) = do
            outDom <- outDomain inDom
            rDownC
                (dispatch outDom)
                ( outName name
                , outDom
                , ConstantAbstract $ AbsLitSet $ map (ConstantAbstract . AbsLitSet) vals
                )
        downC (name, domain, constant) = na $ vcat [ "{downC} PartitionAsSet"
                                                   , "name:" <+> pretty name
                                                   , "domain:" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainPartition "PartitionAsSet" _ _)) =
            case lookup (outName name) ctxt of
                Just (ConstantAbstract (AbsLitSet sets)) -> do
                    let setOut (ConstantAbstract (AbsLitSet xs)) = return xs
                        setOut c = fail $ "Expecting a set, but got:" <+> pretty c
                    vals <- mapM setOut sets
                    return (name, ConstantAbstract (AbsLitPartition vals))
                Nothing -> fail $ vcat $
                    [ "No value for:" <+> pretty (outName name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just constant -> fail $ vcat $
                    [ "Incompatible value for:" <+> pretty (outName name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    , "Expected a set value, but got:" <+> pretty constant
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ (name, domain) = na $ vcat [ "{up} PartitionAsSet"
                                        , "name:" <+> pretty name
                                        , "domain:" <+> pretty domain
                                        ]
