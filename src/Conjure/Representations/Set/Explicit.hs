module Conjure.Representations.Set.Explicit
    ( setExplicit
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Representations.Internal


setExplicit :: MonadFail m => Representation m
setExplicit = Representation chck setDown_ structuralCons setDown setUp

    where

        chck f (DomainSet _ attrs@(SetAttrSize{}) innerDomain) = DomainSet "Explicit" attrs <$> f innerDomain
        chck _ _ = []

        outName name = mconcat [name, "_", "Explicit"]

        setDown_ (name, DomainSet "Explicit" (SetAttrSize size) innerDomain) = return $ Just
            [ ( outName name
              , DomainMatrix
                  (DomainInt [RangeBounded (fromInt 1) size])
                  innerDomain
              ) ]
        setDown_ _ = fail "N/A {setDown_}"

        structuralCons = const $ return Nothing -- TODO: enforce allDiff + sym

        setDown (name, DomainSet "Explicit" (SetAttrSize size) innerDomain, ConstantSet constants) =
            let outIndexDomain = DomainInt [RangeBounded (ConstantInt 1) size]
            in  return $ Just
                    [ ( outName name
                      , DomainMatrix   outIndexDomain innerDomain
                      , ConstantMatrix outIndexDomain constants
                      ) ]
        setDown _ = fail "N/A {setDown}"

        setUp ctxt (name, domain@(DomainSet "Explicit" (SetAttrSize size) innerDomain)) =
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
                                , "With domain:" <+> pretty (DomainSet "Explicit" (SetAttrSize size) innerDomain)
                                ]
        setUp _ _ = fail "N/A {setUp}"

