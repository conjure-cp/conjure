{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Set.Explicit ( setExplicit ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.TH
import Conjure.Language.Pretty
import Conjure.Representations.Internal


setExplicit :: MonadFail m => Representation m
setExplicit = Representation chck downD structuralCons downC up

    where

        chck f (DomainSet _ attrs@(SetAttr SizeAttrSize{}) innerDomain) =
            DomainSet "Explicit" attrs <$> f innerDomain
        chck _ _ = []

        outName name = mconcat [name, "_", "Explicit"]

        downD (name, DomainSet "Explicit" (SetAttr (SizeAttrSize size)) innerDomain) = return $ Just
            [ ( outName name
              , DomainMatrix
                  (DomainInt [RangeBounded (fromInt 1) size])
                  innerDomain
              ) ]
        downD _ = na "{downD} Explicit"

        structuralCons f downX1 (DomainSet "Explicit" (SetAttr (SizeAttrSize size)) innerDomain) = do
            let
                ordering fresh m =
                    let
                        (iPat, i) = quantifiedVar fresh
                    in return -- for list
                        [essence|
                            forAll &iPat : int(1..&size-1) .
                                &m[&i] < &m[&i+1]
                        |]

                innerStructuralCons fresh m = do
                    let (iPat, i) = quantifiedVar (headInf fresh)
                    let activeZone b = [essence| forAll &iPat : int(1..&size) . &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomain

                    let inLoop = [essence| &m[&i] |]
                    refs <- downX1 inLoop
                    outs <- innerStructuralConsGen (tail fresh) refs
                    return (map activeZone outs)

            return $ \ fresh refs ->
                case refs of
                    [m] -> do
                        isc <- innerStructuralCons fresh m
                        return $ concat [ ordering (headInf fresh) m
                                        , isc
                                        ]
                    _ -> na "{structuralCons} Explicit"
        structuralCons _ _ _ = na "{structuralCons} Explicit"

        downC (name, DomainSet "Explicit" (SetAttr (SizeAttrSize size)) innerDomain, ConstantSet constants) =
            let outIndexDomain = DomainInt [RangeBounded (ConstantInt 1) size]
            in  return $ Just
                    [ ( outName name
                      , DomainMatrix   outIndexDomain innerDomain
                      , ConstantMatrix outIndexDomain constants
                      ) ]
        downC _ = na "{downC} Explicit"

        up ctxt (name, domain@(DomainSet "Explicit" (SetAttr (SizeAttrSize _)) _)) =
            case lookup (outName name) ctxt of
                Nothing -> fail $ vcat $
                    [ "No value for:" <+> pretty (outName name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just constant ->
                    case constant of
                        ConstantMatrix _ vals ->
                            return (name, ConstantSet vals)
                        _ -> fail $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (outName name)
                                , "But got:" <+> pretty constant
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
        up _ _ = na "{up} Explicit"

