{-# LANGUAGE Rank2Types #-}

module Conjure.Representations.Relation.RelationAsSet ( relationAsSet ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Representations.Internal


relationAsSet
    :: MonadFail m
    => (forall x . Pretty x => Domain HasRepresentation x -> Representation m)
    -> Representation m
relationAsSet dispatch = Representation chck downD structuralCons downC up

    where

        chck f (DomainRelation _ attrs innerDomains) =
            DomainRelation "RelationAsSet" attrs <$> mapM f innerDomains
        chck _ _ = []

        outName name = mconcat [name, "_", "RelationAsSet"]

        outDomain (DomainRelation "RelationAsSet" (RelationAttr sizeAttr) innerDomains) = do
            let repr = case sizeAttr of
                        SizeAttrSize{} -> "Explicit"
                        _              -> "ExplicitVarSizeWithMarker"
            return (DomainSet repr (SetAttr sizeAttr) (DomainTuple innerDomains))
        outDomain domain = na $ vcat [ "{outDomain} RelationAsSet"
                                     , "domain:" <+> pretty domain
                                     ]

        downD (name, inDom) = do
            outDom <- outDomain inDom
            return $ Just [ ( outName name , outDom ) ]

        structuralCons f downX1 inDom = return $ \ fresh refs ->
            case refs of
                [rel] -> do
                    outDom                 <- outDomain inDom
                    innerStructuralConsGen <- f outDom
                    sets                   <- downX1 rel
                    cons                   <- innerStructuralConsGen fresh sets
                    return cons
                _ -> na $ vcat [ "{structuralCons} RelationAsSet"
                               , pretty inDom
                               ]

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

        up ctxt (name, domain@(DomainRelation "RelationAsSet" _ _)) = do
            case lookup (outName name) ctxt of
                Just (ConstantAbstract (AbsLitSet tuples)) -> do
                    let tupleOut (ConstantAbstract (AbsLitTuple xs)) = return xs
                        tupleOut c = fail $ "Expecting a tuple, but got:" <+> pretty c
                    vals <- mapM tupleOut tuples
                    return (name, ConstantAbstract (AbsLitRelation vals))
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
        up _ (name, domain) = na $ vcat [ "{up} RelationAsMatrix"
                                        , "name:" <+> pretty name
                                        , "domain:" <+> pretty domain
                                        ]
