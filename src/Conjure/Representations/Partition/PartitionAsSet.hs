{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Representations.Partition.PartitionAsSet ( partitionAsSet ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Constant
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal


partitionAsSet
    :: forall m . (MonadFail m, NameGen m)
    => (forall x . Pretty x => Domain HasRepresentation x -> Representation m)
    -> Representation m
partitionAsSet dispatch = Representation chck downD structuralCons downC up

    where

        chck :: TypeOf_ReprCheck m
        chck f _ (DomainPartition _ attrs innerDomain) = do
            innerType <- typeOf innerDomain
            let repr1 = case partsNum attrs of
                        SizeAttr_Size{} -> Set_Explicit                     -- TODO: do not hard-code
                        _               -> Set_ExplicitVarSizeWithMarker
            let repr2 = case partsSize attrs of
                        SizeAttr_Size{} -> Set_Explicit                     -- TODO: do not hard-code
                        _               -> if typesUnify [innerType, TypeInt]
                                             then Set_Occurrence
                                             else Set_ExplicitVarSizeWithMarker
            innerDomain' <- f innerDomain
            return [ DomainPartition (Partition_AsSet repr1 repr2) attrs d | d <- innerDomain' ]
        chck _ _ _ = return []

        outName :: Name -> Name
        outName = mkOutName (Partition_AsSet def def) Nothing

        outDomain :: Pretty x => Domain HasRepresentation x -> m (Domain HasRepresentation x)
        outDomain (DomainPartition (Partition_AsSet repr1 repr2) (PartitionAttr{..}) innerDomain) =
            return (DomainSet repr1 (SetAttr partsNum) (DomainSet repr2 (SetAttr partsSize) innerDomain))
        outDomain domain = na $ vcat [ "{outDomain} PartitionAsSet"
                                     , "domain:" <+> pretty domain
                                     ]

        downD :: TypeOf_DownD m
        downD (name, inDom) = do
            outDom <- outDomain inDom
            return $ Just [ ( outName name , outDom ) ]

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 inDom@(DomainPartition _ attrs innerDomain) = return $ \ inpRel -> do
            refs <- downX1 inpRel
            let
                exactlyOnce rel = do
                    (iPat, i) <- quantifiedVar
                    (jPat, j) <- quantifiedVar
                    return $ return $ -- for list
                        [essence|
                            forAll &iPat : &innerDomain .
                                1  = sum ([ 1
                                          | &jPat <- &rel
                                          , &i in &j
                                          ])
                                |]

                regular rel | isRegular attrs = do
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

            case refs of
                [rel] -> do
                    outDom                 <- outDomain inDom
                    innerStructuralConsGen <- f outDom
                    concat <$> sequence
                        [ exactlyOnce rel
                        , regular rel
                        , partsAren'tEmpty rel
                        , innerStructuralConsGen rel
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
        up ctxt (name, domain@(DomainPartition Partition_AsSet{} _ _)) =
            case lookup (outName name) ctxt of
                Nothing -> fail $ vcat $
                    [ "(in PartitionAsSet up)"
                    , "No value for:" <+> pretty (outName name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just (viewConstantSet -> Just sets) -> do
                    let setOut (viewConstantSet -> Just xs) = return xs
                        setOut c = fail $ "Expecting a set, but got:" <+> pretty c
                    vals <- mapM setOut sets
                    return (name, ConstantAbstract (AbsLitPartition vals))
                Just (ConstantUndefined msg ty) ->        -- undefined propagates
                    return (name, ConstantUndefined ("PartitionAsSet " `mappend` msg) ty)
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
