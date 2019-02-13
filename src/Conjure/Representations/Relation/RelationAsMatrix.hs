{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Relation.RelationAsMatrix ( relationAsMatrix ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainValues )


relationAsMatrix :: forall m . (MonadFail m, NameGen m) => Representation m
relationAsMatrix = Representation chck downD structuralCons downC up symmetryOrdering

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainRelation _ attrs innerDomains) | all domainCanIndexMatrix innerDomains =
            map (DomainRelation Relation_AsMatrix attrs) . sequence <$> mapM f innerDomains
        chck _ _ = return []

        outName :: Domain HasRepresentation x -> Name -> Name
        outName = mkOutName Nothing

        downD :: TypeOf_DownD m
        downD (name, domain@(DomainRelation Relation_AsMatrix _ innerDomains))
                        | all domainCanIndexMatrix innerDomains = do
            let unroll is j = foldr DomainMatrix j is
            return $ Just
                [ ( outName domain name
                  , unroll (map forgetRepr innerDomains) DomainBool
                  ) ]
        downD (name, domain) = na $ vcat [ "{downD} RelationAsMatrix"
                                         , "name:" <+> pretty name
                                         , "domain:" <+> pretty domain
                                         ]

        structuralCons :: TypeOf_Structural m
        structuralCons _ downX1
            (DomainRelation Relation_AsMatrix (RelationAttr sizeAttr binRelAttr) innerDomains)
                | all domainCanIndexMatrix innerDomains = do
            let cardinality m = do
                    let unroll _ [] = fail "RelationAsMatrix.cardinality.unroll []"
                        unroll n [dom] = do
                            (iPat, i) <- quantifiedVar
                            return [essence| sum &iPat : &dom . toInt(&n[&i]) |]
                        unroll n (dom : rest) = do
                            (iPat, i) <- quantifiedVar
                            r <- unroll [essence| &n[&i] |] rest
                            return [essence| sum &iPat : &dom . &r |]
                    unroll m innerDomains
            return $ \ rel -> do
                refs <- downX1 rel
                case refs of
                    [m] -> do
                        binRelCons <- if binRelAttr == def then return [] else
                            case innerDomains of
                                [innerDomain1, innerDomain2]
                                    | forgetRepr innerDomain1 == forgetRepr innerDomain2 ->
                                        mkBinRelCons binRelAttr innerDomain1 rel
                                    | otherwise ->
                                          bug $ vcat [ "Binary relation between different domains. (RelationAsMatrix)"
                                                     , "innerDomain1:" <+> pretty innerDomain1
                                                     , "innerDomain2:" <+> pretty innerDomain2
                                                     ]
                                _      -> bug "Non-binary relation."
                        concat <$> sequence
                            [ mkSizeCons sizeAttr <$> cardinality m
                            , return binRelCons
                            ]
                    _ -> na "{structuralCons} RelationAsMatrix"
        structuralCons _ _ _ = na "{structuralCons} RelationAsMatrix"

        downC :: TypeOf_DownC m
        downC ( name
              , domain@(DomainRelation Relation_AsMatrix _ innerDomains)
              , ConstantAbstract (AbsLitRelation vals)
              ) | all domainCanIndexMatrix innerDomains = do
            let
                check :: [Constant] -> Bool
                check indices = indices `elem` vals

            let
                unrollD :: [Domain () Constant] -> Domain r Constant -> Domain r Constant
                unrollD is j = foldr DomainMatrix j is

            let
                unrollC :: MonadFail m
                        => [Domain () Constant]
                        -> [Constant]               -- indices
                        -> m Constant
                unrollC [i] prevIndices = do
                    domVals <- domainValues i
                    return $ ConstantAbstract $ AbsLitMatrix i
                        [ ConstantBool $ check $ prevIndices ++ [val]
                        | val <- domVals ]
                unrollC (i:is) prevIndices = do
                    domVals <- domainValues i
                    matrixVals <- forM domVals $ \ val ->
                        unrollC is (prevIndices ++ [val])
                    return $ ConstantAbstract $ AbsLitMatrix i matrixVals
                unrollC is prevIndices = fail $ vcat [ "RelationAsMatrix.up.unrollC"
                                                     , "    is         :" <+> vcat (map pretty is)
                                                     , "    prevIndices:" <+> pretty (show prevIndices)
                                                     ]

            outConstant <- unrollC (map forgetRepr innerDomains) []

            return $ Just
                [ ( outName domain name
                  , unrollD (map forgetRepr innerDomains) DomainBool
                  , outConstant
                  ) ]

        downC (name, domain, constant) = na $ vcat [ "{downC} RelationAsMatrix"
                                                   , "name:" <+> pretty name
                                                   , "domain:" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainRelation Relation_AsMatrix _ innerDomains)) =

            case lookup (outName domain name) ctxt of
                Nothing -> fail $ vcat $
                    [ "(in RelationAsMatrix up)"
                    , "No value for:" <+> pretty (outName domain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just constant -> do
                    let
                        allIndices :: (MonadFail m, Pretty r) => [Domain r Constant] -> m [[Constant]]
                        allIndices = fmap sequence . mapM domainValues

                        index :: MonadFail m => Constant -> [Constant] -> m Constant
                        index m [] = return m
                        index (ConstantAbstract (AbsLitMatrix indexDomain vals)) (i:is) = do
                            froms <- domainValues indexDomain
                            case lookup i (zip froms vals) of
                                Nothing -> fail "Value not found. RelationAsMatrix.up.index"
                                Just v  -> index v is
                        index m is = fail ("RelationAsMatrix.up.index" <+> pretty m <+> pretty (show is))

                    indices  <- allIndices innerDomains
                    vals     <- forM indices $ \ these -> do
                        indexed <- index constant these
                        case viewConstantBool indexed of
                            Just False -> return Nothing
                            Just True  -> return (Just these)
                            _ -> fail $ vcat
                                [ "Expecting a boolean literal, but got:" <++> pretty indexed
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]

                    return ( name
                           , ConstantAbstract $ AbsLitRelation $ catMaybes vals
                           )
        up _ (name, domain) = na $ vcat [ "{up} RelationAsMatrix"
                                        , "name:" <+> pretty name
                                        , "domain:" <+> pretty domain
                                        ]

        symmetryOrdering :: TypeOf_SymmetryOrdering m
        symmetryOrdering innerSO downX1 inp domain = do
            [inner] <- downX1 inp
            Just [(_, innerDomain)] <- downD ("SO", domain)
            innerSO downX1 inner innerDomain
