{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}

module Conjure.Representations.Relation.RelationAsSet ( relationAsSet ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Representations.Internal
import Conjure.Representations.Common


relationAsSet
    :: forall m . (MonadFail m, NameGen m)
    => (forall x . DispatchFunction m x)
    -> (forall r x . ReprOptionsFunction m r x)
    -> Bool
    -> Representation m
relationAsSet dispatch reprOptions useLevels = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck _ dom1@(DomainRelation _ attrs _) = do
            dom2 <- outDomain_ dom1
            dom3 <- reprOptions dom2
            return [ DomainRelation (Relation_AsSet r) attrs innerDomains
                   | DomainSet r _ (DomainTuple innerDomains) <- dom3
                   -- special hack: do not use Set_ExplicitVarSizeWithFlags when --representation-levels=yes
                   , if useLevels
                       then r /= Set_ExplicitVarSizeWithFlags
                       else True
                   ]
        chck _ _ = return []

        outName :: Domain HasRepresentation x -> Name -> Name
        outName = mkOutName Nothing

        outDomain_ :: Pretty x => Domain () x -> m (Domain () x)
        outDomain_ (DomainRelation () (RelationAttr sizeAttr _binRelAttrs) innerDomains) =
            return (DomainSet () (SetAttr sizeAttr) (DomainTuple innerDomains))
        outDomain_ domain = na $ vcat [ "{outDomain_} RelationAsSet"
                                      , "domain:" <+> pretty domain
                                      ]

        outDomain :: Pretty x => Domain HasRepresentation x -> m (Domain HasRepresentation x)
        outDomain (DomainRelation (Relation_AsSet repr) (RelationAttr sizeAttr _binRelAttrs) innerDomains) =
            return (DomainSet repr (SetAttr sizeAttr) (DomainTuple innerDomains))
        outDomain domain = na $ vcat [ "{outDomain} RelationAsSet"
                                     , "domain:" <+> pretty domain
                                     ]

        downD :: TypeOf_DownD m
        downD (name, inDom) = do
            outDom <- outDomain inDom
            return $ Just [ ( outName inDom name , outDom ) ]

        structuralCons :: TypeOf_Structural m
        structuralCons f downX1 inDom = do
            let
                innerStructuralCons rel = do
                    outDom                 <- outDomain inDom
                    innerStructuralConsGen <- f outDom
                    innerStructuralConsGen rel

            return $ \ rel -> do
                refs <- downX1 rel
                case refs of
                    [set] -> do
                        binRelCons <- case inDom of
                            DomainRelation Relation_AsSet{} (RelationAttr _ binRelAttrs) [innerDomain1, innerDomain2]
                                | binRelAttrs == def
                                    -> return []
                                | forgetRepr innerDomain1 == forgetRepr innerDomain2
                                    -> mkBinRelCons binRelAttrs innerDomain1 rel
                                | otherwise
                                    -> bug $ vcat [ "Binary relation between different domains. (RelationAsSet)"
                                                  , "innerDomain1:" <+> pretty innerDomain1
                                                  , "innerDomain2:" <+> pretty innerDomain2
                                                  ]
                            DomainRelation Relation_AsSet{} (RelationAttr _ binRelAttrs) innerDomains
                                | length innerDomains /= 2 && binRelAttrs /= def
                                    -> bug "Non-binary relation has binary relation attributes."
                            _ -> return []
                        concat <$> sequence
                            [ innerStructuralCons set
                            , return binRelCons
                            ]
                    _ -> na $ vcat [ "{structuralCons} RelationAsSet"
                                   , pretty inDom
                                   ]

        downC :: TypeOf_DownC m
        downC ( name
              , inDom
              , ConstantAbstract (AbsLitRelation vals)
              ) = do
            outDom <- outDomain inDom
            rDownC
                (dispatch outDom)
                ( outName inDom name
                , outDom
                , ConstantAbstract $ AbsLitSet $ map (ConstantAbstract . AbsLitTuple) vals
                )
        downC (name, domain, constant) = na $ vcat [ "{downC} RelationAsSet"
                                                   , "name:" <+> pretty name
                                                   , "domain:" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainRelation Relation_AsSet{} _ _)) =
            case lookup (outName domain name) ctxt of
                Just (ConstantAbstract (AbsLitSet tuples)) -> do
                    vals <- mapM viewConstantTuple tuples
                    return (name, ConstantAbstract (AbsLitRelation vals))
                Nothing -> fail $ vcat $
                    [ "(in RelationAsSet up)"
                    , "No value for:" <+> pretty (outName domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just constant -> fail $ vcat $
                    [ "Incompatible value for:" <+> pretty (outName domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    , "Expected a set value, but got:" <++> pretty constant
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ (name, domain) = na $ vcat [ "{up} RelationAsSet"
                                        , "name:" <+> pretty name
                                        , "domain:" <+> pretty domain
                                        ]

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do
            [inner] <- downX1 inp
            Just [(_, innerDomain)] <- downD ("SO", domain)
            innerSO downX1 inner innerDomain
