{-# LANGUAGE Rank2Types #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Representations.Relation.RelationAsSet ( relationAsSet ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Constant
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Representations.Internal
import Conjure.Representations.Common


relationAsSet
    :: forall m . (MonadFail m, NameGen m)
    => (forall x . Pretty x => Domain HasRepresentation x -> Representation m)
    -> Representation m
relationAsSet dispatch = Representation chck downD structuralCons downC up

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainRelation _ attrs innerDomains) =
            map (DomainRelation Relation_AsSet attrs) . sequence <$> mapM f innerDomains
        chck _ _ = return []

        outName :: Name -> Name
        outName = mkOutName Relation_AsSet Nothing

        outDomain (DomainRelation Relation_AsSet (RelationAttr sizeAttr _binRelAttrs) innerDomains) = do
            let repr = case sizeAttr of
                        SizeAttr_Size{} -> Set_Explicit                     -- TODO: do not hard-code
                        _               -> Set_ExplicitVarSizeWithMarker
            return (DomainSet repr (SetAttr sizeAttr) (DomainTuple innerDomains))
        outDomain domain = na $ vcat [ "{outDomain} RelationAsSet"
                                     , "domain:" <+> pretty domain
                                     ]

        downD :: TypeOf_DownD m
        downD (name, inDom) = do
            outDom <- outDomain inDom
            return $ Just [ ( outName name , outDom ) ]

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
                            DomainRelation Relation_AsSet (RelationAttr _ binRelAttrs) [innerDomain1, innerDomain2]
                                | binRelAttrs == def
                                    -> return []
                                | forgetRepr innerDomain1 == forgetRepr innerDomain2
                                    -> mkBinRelCons binRelAttrs innerDomain1 rel
                                | otherwise
                                    -> bug $ vcat [ "Binary relation between different domains. (RelationAsSet)"
                                                  , "innerDomain1:" <+> pretty innerDomain1
                                                  , "innerDomain2:" <+> pretty innerDomain2
                                                  ]
                            DomainRelation Relation_AsSet (RelationAttr _ binRelAttrs) innerDomains
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
                ( outName name
                , outDom
                , ConstantAbstract $ AbsLitSet $ map (ConstantAbstract . AbsLitTuple) vals
                )
        downC (name, domain, constant) = na $ vcat [ "{downC} RelationAsSet"
                                                   , "name:" <+> pretty name
                                                   , "domain:" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainRelation Relation_AsSet _ _)) =
            case lookup (outName name) ctxt of
                Just (ConstantAbstract (AbsLitSet tuples)) -> do
                    let tupleOut (viewConstantTuple -> Just xs) = return xs
                        tupleOut c = fail $ "Expecting a tuple, but got:" <+> pretty c
                    vals <- mapM tupleOut tuples
                    return (name, ConstantAbstract (AbsLitRelation vals))
                Nothing -> fail $ vcat $
                    [ "(in RelationAsSet up)"
                    , "No value for:" <+> pretty (outName name)
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
        up _ (name, domain) = na $ vcat [ "{up} RelationAsSet"
                                        , "name:" <+> pretty name
                                        , "domain:" <+> pretty domain
                                        ]
