{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE Rank2Types #-}

module Conjure.Representations.Function.FunctionAsRelation ( functionAsRelation ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal


functionAsRelation
    :: MonadFail m
    => (forall x . Pretty x => Domain HasRepresentation x -> Representation m)
    -> Representation m
functionAsRelation dispatch = Representation chck downD structuralCons downC up

    where

        chck f (DomainFunction _ attrs innerDomainFr innerDomainTo) =
            DomainFunction "FunctionAsRelation" attrs <$> f innerDomainFr <*> f innerDomainTo
        chck _ _ = []

        outName name = mconcat [name, "_", "FunctionAsRelation"]

        outDomain (DomainFunction "FunctionAsRelation"
                    (FunctionAttr sizeAttr _partilityAttr _jectivityAttr)
                    innerDomainFr innerDomainTo) = do
            let repr = if all domainCanIndexMatrix [innerDomainFr, innerDomainTo]
                        then "RelationAsMatrix"
                        else "RelationAsSet"
            return (DomainRelation repr (RelationAttr sizeAttr def) [innerDomainFr, innerDomainTo])
        outDomain domain = na $ vcat [ "{outDomain} FunctionAsRelation"
                                     , "domain:" <+> pretty domain
                                     ]

        downD (name, inDom) = do
            outDom <- outDomain inDom
            return $ Just [ ( outName name , outDom ) ]

        structuralCons f downX1
                inDom@(DomainFunction _
                        (FunctionAttr _ partilityAttr jectivityAttr)
                        innerDomainFr innerDomainTo) = return $ \ fresh func -> do
            refs <- downX1 func
            let
                partialityCons rel = return $ -- list
                    case partilityAttr of
                        PartialityAttr_Partial ->
                            let
                                (iPat, i) = quantifiedVar (fresh `at` 0)
                                (jPat, j) = quantifiedVar (fresh `at` 1)
                            in
                                [essence|
                                    forAll &iPat : &innerDomainFr .
                                        1 >= sum([ 1
                                                | &jPat <- &rel
                                                , &j[1] = &i
                                                ])
                                |]
                        PartialityAttr_Total ->
                            let
                                (iPat, i) = quantifiedVar (fresh `at` 0)
                                (jPat, j) = quantifiedVar (fresh `at` 1)
                            in
                                [essence|
                                    forAll &iPat : &innerDomainFr .
                                        1 =  sum([ 1
                                                | &jPat <- &rel
                                                , &j[1] = &i
                                                ])
                                |]

                jectivityCons rel = case jectivityAttr of
                    JectivityAttr_None       -> []
                    JectivityAttr_Injective  -> injectiveCons rel
                    JectivityAttr_Surjective -> surjectiveCons rel
                    JectivityAttr_Bijective  -> injectiveCons rel ++ surjectiveCons rel

                injectiveCons rel = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)
                    in
                        [essence|
                            and([ &i[1] != &j[1] -> &i[2] != &j[2]
                                | &iPat <- &rel
                                , &jPat <- &rel
                                ])
                        |]

                surjectiveCons rel = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)
                    in
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat in &rel .
                                    &j[2] = &i
                        |]

            case refs of
                [rel] -> do
                    outDom                 <- outDomain inDom
                    innerStructuralConsGen <- f outDom
                    cons                   <- innerStructuralConsGen fresh rel
                    return $ concat
                        [ partialityCons rel
                        , jectivityCons rel
                        , cons
                        ]
                _ -> na $ vcat [ "{structuralCons} FunctionAsRelation"
                               , pretty inDom
                               ]
        structuralCons _ _ inDom =
            na $ vcat [ "{structuralCons} FunctionAsRelation"
                      , pretty inDom
                      ]

        downC ( name
              , inDom
              , ConstantAbstract (AbsLitFunction vals)
              ) = do
            outDom <- outDomain inDom
            rDownC
                (dispatch outDom)
                ( outName name
                , outDom
                , ConstantAbstract $ AbsLitRelation $ map (\ (a,b) -> [a,b] ) vals
                )
        downC (name, domain, constant) = na $ vcat [ "{downC} FunctionAsRelation"
                                                   , "name:" <+> pretty name
                                                   , "domain:" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]

        up ctxt (name, domain@(DomainFunction "FunctionAsRelation" _ _ _)) =
            case lookup (outName name) ctxt of
                Just (ConstantAbstract (AbsLitRelation pairs)) -> do
                    let pairOut [a,b] = return (a,b)
                        pairOut c = fail $ "Expecting a 2-tuple, but got:" <+> prettyList prParens "," c
                    vals <- mapM pairOut pairs
                    return (name, ConstantAbstract (AbsLitFunction vals))
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
        up _ (name, domain) = na $ vcat [ "{up} FunctionAsRelation"
                                        , "name:" <+> pretty name
                                        , "domain:" <+> pretty domain
                                        ]
