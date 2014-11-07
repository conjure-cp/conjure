{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Set.Occurrence ( setOccurrence ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Language.DomainSize ( valuesInIntDomain )
import Conjure.Representations.Internal
import Conjure.Representations.Common


setOccurrence :: MonadFail m => Representation m
setOccurrence = Representation chck downD structuralCons downC up

    where

        chck f (DomainSet _ attrs innerDomain@(DomainInt{})) = DomainSet "Occurrence" attrs <$> f innerDomain
        chck _ _ = []

        outName name = mconcat [name, "_", "Occurrence"]

        downD (name, DomainSet "Occurrence" _attrs innerDomain@DomainInt{}) = return $ Just
            [ ( outName name
              , DomainMatrix (forgetRepr innerDomain) DomainBool
              )
            ]
        downD _ = na "{downD} Occurrence"

        structuralCons _ _ (DomainSet "Occurrence" (SetAttr attrs) innerDomain@DomainInt{}) =
            return $ \ fresh refs ->
                case refs of
                    [m] -> do
                        let (iPat, i) = quantifiedVar (fresh `at` 0) TypeInt
                            cardinality = [essence| sum &iPat : &innerDomain . &m[&i] |]
                        return (mkSizeCons attrs cardinality)
                    _ -> na "{structuralCons} Occurrence"
        structuralCons _ _ _ = na "{structuralCons} Occurrence"

        downC (name, DomainSet "Occurrence" _attrs innerDomain@(DomainInt intRanges), ConstantSet constants) = do
                innerDomainVals <- valuesInIntDomain intRanges
                return $ Just
                    [ ( outName name
                      , DomainMatrix   (forgetRepr innerDomain) DomainBool
                      , ConstantMatrix (forgetRepr innerDomain)
                          [ ConstantBool isIn
                          | v <- innerDomainVals
                          , let isIn = ConstantInt v `elem` constants
                          ]
                      )
                    ]
        downC _ = na "{downC} Occurrence"

        up ctxt (name, domain@(DomainSet _ _ (DomainInt intRanges)))=
            case lookup (outName name) ctxt of
                Just constantMatrix ->
                    case constantMatrix of
                        ConstantMatrix _ vals -> do
                            innerDomainVals <- valuesInIntDomain intRanges
                            return (name, ConstantSet
                                            [ ConstantInt v
                                            | (v,b) <- zip innerDomainVals vals
                                            , b == ConstantBool True
                                            ] )
                        _ -> fail $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (outName name)
                                , "But got:" <+> pretty constantMatrix
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                Nothing -> fail $ vcat $
                    [ "No value for:" <+> pretty (outName name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up} Occurrence"

