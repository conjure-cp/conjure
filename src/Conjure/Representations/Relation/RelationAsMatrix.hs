{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Relation.RelationAsMatrix ( relationAsMatrix ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainValues, toIntDomain )


relationAsMatrix :: forall m . MonadFail m => Representation m
relationAsMatrix = Representation chck downD structuralCons downC up

    where

        chck :: TypeOf_ReprCheck m
        chck f (DomainRelation _ attrs innerDomains) | all domainCanIndexMatrix innerDomains =
            DomainRelation "RelationAsMatrix" attrs <$> mapM f innerDomains
        chck _ _ = []

        outName name = mconcat [name, "_", "RelationAsMatrix"]

        downD :: TypeOf_DownD m
        downD (name, DomainRelation "RelationAsMatrix" _ innerDomains') | all domainCanIndexMatrix innerDomains' = do
            innerDomains <- mapM toIntDomain innerDomains'
            let unroll is j = foldr DomainMatrix j is
            return $ Just
                [ ( outName name
                  , unroll (map (forgetRepr "Representation.RelationAsMatrix") innerDomains) DomainBool
                  ) ]
        downD (name, domain) = na $ vcat [ "{downD} RelationAsMatrix"
                                         , "name:" <+> pretty name
                                         , "domain:" <+> pretty domain
                                         ]

        structuralCons :: TypeOf_Structural m
        structuralCons _ downX1
            (DomainRelation "RelationAsMatrix" (RelationAttr sizeAttr binRelAttr) innerDomains')
                | all domainCanIndexMatrix innerDomains' = do
            innerDomains <- mapM toIntDomain innerDomains'
            let cardinality fresh m =
                    let unroll _ [] = bug "RelationAsMatrix.cardinality.unroll []"
                        unroll n [((iPat, i), dom)] =
                                [essence| sum &iPat : &dom . toInt(&n[&i]) |]
                        unroll n (((iPat, i), dom) : rest) =
                            let r = unroll [essence| &n[&i] |] rest
                            in  [essence| sum &iPat : &dom . &r |]
                    in  unroll m (zip [ quantifiedVar f | f <- fresh ] innerDomains)
            return $ \ fresh rel -> do
                refs <- downX1 rel
                case refs of
                    [m] -> do
                        binRelCons <- if binRelAttr == def then return [] else
                            case innerDomains of
                                [innerDomain1, innerDomain2] | innerDomain1 == innerDomain2 ->
                                    return (mkBinRelCons binRelAttr fresh innerDomain1 rel)
                                [_, _] -> fail "Binary relation between different domains."
                                _      -> fail "Non-binary relation."
                        return $ concat
                                    [ mkSizeCons sizeAttr (cardinality fresh m)
                                    , binRelCons
                                    ]
                    _ -> na "{structuralCons} RelationAsMatrix"
        structuralCons _ _ _ = na "{structuralCons} RelationAsMatrix"

        downC :: TypeOf_DownC m
        downC ( name
              , DomainRelation "RelationAsMatrix" _ innerDomains'
              , ConstantAbstract (AbsLitRelation vals)
              ) | all domainCanIndexMatrix innerDomains' = do
            innerDomains <- fmap (fmap e2c) <$> mapM toIntDomain (fmap (fmap Constant) innerDomains')
            let
                check :: [Constant] -> Bool
                check indices = indices `elem` vals

            let
                unrollD :: [Domain () Constant] -> Domain r Constant -> Domain r Constant
                unrollD is j = foldr DomainMatrix j is

            let
                unrollC :: MonadFail m
                        => [ ( Domain () Constant       -- the int domain
                             , Domain () Constant       -- the actial domain
                             ) ]
                        -> [Constant]               -- indices
                        -> m Constant
                unrollC [(i,i')] prevIndices = do
                    domVals <- domainValues i'
                    return $ ConstantAbstract $ AbsLitMatrix i
                        [ ConstantBool $ check $ prevIndices ++ [val]
                        | val <- domVals ]
                unrollC ((i,i'):is) prevIndices = do
                    domVals <- domainValues i'
                    matrixVals <- forM domVals $ \ val ->
                        unrollC is (prevIndices ++ [val])
                    return $ ConstantAbstract $ AbsLitMatrix i matrixVals
                unrollC is prevIndices = fail $ vcat [ "RelationAsMatrix.up.unrollC"
                                                     , "    is         :" <+> vcat (map pretty is)
                                                     , "    prevIndices:" <+> pretty (show prevIndices)
                                                     ]

            outConstant <- unrollC (zip (map (forgetRepr "Representation.RelationAsMatrix") innerDomains)
                                        (map (forgetRepr "Representation.RelationAsMatrix") innerDomains')) []

            return $ Just
                [ ( outName name
                  , unrollD (map (forgetRepr "Representation.RelationAsMatrix") innerDomains) DomainBool
                  , outConstant
                  ) ]

        downC (name, domain, constant) = na $ vcat [ "{downC} RelationAsMatrix"
                                                   , "name:" <+> pretty name
                                                   , "domain:" <+> pretty domain
                                                   , "constant:" <+> pretty constant
                                                   ]

        up :: TypeOf_Up m
        up ctxt (name, domain@(DomainRelation "RelationAsMatrix" _ innerDomains')) = do

            innerDomains <- fmap (fmap e2c) <$> mapM toIntDomain (fmap (fmap Constant) innerDomains')
            
            case lookup (outName name) ctxt of
                Nothing -> fail $ vcat $
                    [ "No value for:" <+> pretty (outName name)
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
                        index m is = bug ("RelationAsMatrix.up.index" <+> pretty m <+> pretty (show is))

                    indices' <- allIndices innerDomains'
                    indices  <- allIndices innerDomains
                    vals     <- forM (zip indices indices') $ \ (these, these') -> do
                        indexed <- index constant these
                        case indexed of
                            ConstantBool False -> return Nothing
                            ConstantBool True  -> return (Just these')
                            _ -> fail $ vcat
                                [ "Expecting a boolean literal, but got:" <+> pretty indexed
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

