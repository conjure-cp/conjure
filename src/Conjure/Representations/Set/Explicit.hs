module Conjure.Representations.Set.Explicit
    ( setExplicit
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Lenses
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
        downD _ = fail "N/A {downD}"

        structuralCons (name, DomainSet "Explicit" (SetAttr (SizeAttrSize size)) innerDomain@DomainInt{}) =
            return $ Just $ \ fresh -> return $
            let
                m = Reference (outName name)
                              (Just (DeclHasRepr
                                          Find
                                          (outName name)
                                          (DomainMatrix (DomainInt [RangeBounded (fromInt 1) size]) innerDomain)))
                iName = headInf fresh
                body = mkLambda iName TypeInt $ \ i -> make opLt (make opIndexing m i)
                                                                 (make opIndexing m (make opPlus i (fromInt 1)))
            in
                make opAnd [
                    make opMapOverDomain body
                        (Domain $ DomainInt [RangeBounded (fromInt 1) (make opMinus size (fromInt 1))])
                ]
        structuralCons _ = fail "N/A {structuralCons}"

        downC (name, DomainSet "Explicit" (SetAttr (SizeAttrSize size)) innerDomain, ConstantSet constants) =
            let outIndexDomain = DomainInt [RangeBounded (ConstantInt 1) size]
            in  return $ Just
                    [ ( outName name
                      , DomainMatrix   outIndexDomain innerDomain
                      , ConstantMatrix outIndexDomain constants
                      ) ]
        downC _ = fail "N/A {downC}"

        up ctxt (name, domain@(DomainSet "Explicit" (SetAttr (SizeAttrSize size)) innerDomain)) =
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
                                , "With domain:" <+> pretty (DomainSet "Explicit" (SetAttr (SizeAttrSize size)) innerDomain)
                                ]
        up _ _ = fail "N/A {up}"

