{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.MSet.Occurrence ( msetOccurrence ) where

-- conjure
import Conjure.Prelude
import Conjure.Language
import Conjure.Representations.Internal
import Conjure.Representations.Common


msetOccurrence :: forall m . (MonadFailDoc m, NameGen m) => Representation m
msetOccurrence = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainMSet _ attrs innerDomain@DomainInt{}) = map (DomainMSet MSet_Occurrence attrs) <$> f innerDomain
        chck _ _ = return []

        outName :: Domain HasRepresentation x -> Name -> Name
        outName = mkOutName Nothing

        getMinOccur attrs = case attrs of
            MSetAttr _ (OccurAttr_MinOccur x) -> x
            MSetAttr _ (OccurAttr_MinMaxOccur x _) -> x
            MSetAttr _ _ -> 0

        getMaxOccur attrs = case attrs of
            MSetAttr _ (OccurAttr_MaxOccur x) -> return x
            MSetAttr _ (OccurAttr_MinMaxOccur _ x) -> return x
            MSetAttr (SizeAttr_Size x) _ -> return x
            MSetAttr (SizeAttr_MaxSize x) _ -> return x
            MSetAttr (SizeAttr_MinMaxSize _ x) _ -> return x
            _ -> failDoc ("getMaxOccur, mset not supported. attributes:" <+> pretty attrs)

        downD :: TypeOf_DownD m
        downD (name, domain@(DomainMSet MSet_Occurrence attrs innerDomain@DomainInt{})) = do
            maxOccur <- getMaxOccur attrs
            return $ Just
                [ ( outName domain name
                  , DomainMatrix (forgetRepr innerDomain) (DomainInt TagInt [RangeBounded 0 maxOccur])
                  )
                ]
        downD _ = na "{downD} Occurrence"

        structuralCons :: TypeOf_Structural m
        structuralCons _ downX1 (DomainMSet MSet_Occurrence
                                    attrs@(MSetAttr sizeAttr _occurAttr)
                                    innerDomain@DomainInt{}) =
            return $ \ mset -> do
                refs <- downX1 mset
                case refs of
                    [m] -> do
                        (iPat, i) <- quantifiedVar
                        let
                            minOccur = getMinOccur attrs
                            minOccurCons =
                                [ [essence| forAll &iPat : &innerDomain . &m[&i] >= &minOccur |]
                                | minOccur /= 0 ]
                        let
                            cardinality = [essence| sum &iPat : &innerDomain . &m[&i] |]
                            cardinalityCons = mkSizeCons sizeAttr cardinality
                        return (minOccurCons ++ cardinalityCons)
                    _ -> na "{structuralCons} Occurrence"
        structuralCons _ _ _ = na "{structuralCons} Occurrence"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainMSet MSet_Occurrence _attrs innerDomain@(DomainInt t intRanges))
              , viewConstantMSet -> Just constants
              ) = do
                innerDomainVals <- valuesInIntDomain intRanges
                return $ Just
                    [ ( outName domain name
                      , DomainMatrix (forgetRepr innerDomain) DomainBool
                      , ConstantAbstract $ AbsLitMatrix (forgetRepr innerDomain)
                          [ ConstantBool isIn
                          | v <- innerDomainVals
                          , let isIn = ConstantInt t v `elem` constants
                          ]
                      )
                    ]
        downC _ = na "{downC} Occurrence"

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainMSet _ _ (DomainInt t intRanges)))=
            case lookup (outName domain name) ctxt of
                Just constantMatrix ->
                    case viewConstantMatrix constantMatrix of
                        Just (_, vals) -> do
                            innerDomainVals <- valuesInIntDomain intRanges
                            return (name, ConstantAbstract $ AbsLitMSet $ concat
                                            [ case viewConstantInt x of
                                                Just n -> replicate (fromInteger n) (ConstantInt t v)
                                                Nothing -> []
                                            | (v,x) <- zip innerDomainVals vals
                                            ] )
                        _ -> failDoc $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (outName domain name)
                                , "But got:" <+> pretty constantMatrix
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                Nothing -> failDoc $ vcat $
                    [ "(in MSet Occurrence up)"
                    , "No value for:" <+> pretty (outName domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up} Occurrence"

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do 
            [inner] <- downX1 inp
            Just [(_, innerDomain)] <- downD ("SO", domain)
            innerSO downX1 inner innerDomain

