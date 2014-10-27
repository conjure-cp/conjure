module Conjure.Representations.Function.Function1D
    ( function1D
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.DomainSize
-- import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.Lenses
-- import Conjure.Language.Pretty
import Conjure.Representations.Internal


function1D :: MonadFail m => Representation m
function1D = Representation chck downD structuralCons downC up

    where

        chck f (DomainFunction _
                    attrs@(FunctionAttr _ FunctionAttr_Total _)
                    innerDomainFr@DomainInt{}
                    innerDomainTo) =
            DomainFunction "Matrix1D" attrs
                <$> f innerDomainFr
                <*> f innerDomainTo
        chck _ _ = []

        outName name = mconcat [name, "_", "Function1D"]

        downD (name, DomainFunction _
                    (FunctionAttr _ FunctionAttr_Total _)
                    innerDomainFr@DomainInt{}
                    innerDomainTo) = return $ Just
            [ ( outName name
              , DomainMatrix
                  (forgetRepr innerDomainFr)
                  innerDomainTo
              ) ]
        downD _ = fail "N/A {downD}"

        structuralCons (name, DomainFunction _
                    (FunctionAttr sizeAttr FunctionAttr_Total jectivityAttr)
                    innerDomainFr@DomainInt{}
                    innerDomainTo) = do

            let m = Reference (outName name)
                              (Just (DeclHasRepr
                                          Find
                                          (outName name)
                                          (DomainMatrix (forgetRepr innerDomainFr) innerDomainTo)))

            innerDomainFrTy <- typeOf innerDomainFr
            innerDomainToTy <- typeOf innerDomainTo

            let injectiveCons = const [make opAllDiff m]
            let surjectiveCons fresh = return $ -- list
                    let
                        iName = fresh `at` 0
                        jName = fresh `at` 1
                    in
                        -- forAll &i : &innerDomainTo .
                        --     exists &j : &innerDomainFr .
                        --         &m[&j] = &i
                        make opAnd [
                            make opMapOverDomain
                                (mkLambda iName innerDomainToTy $ \ i ->
                                    make opOr [
                                        make opMapOverDomain
                                            (mkLambda jName innerDomainFrTy $ \ j ->
                                                make opEq
                                                    (make opIndexing m j)
                                                    (i))
                                            (Domain (forgetRepr innerDomainFr))
                                    ]
                                )
                                (Domain (forgetRepr innerDomainTo))       
                        ]

            let jectivityCons = case jectivityAttr of
                    ISBAttr_None       -> const []
                    ISBAttr_Injective  -> injectiveCons
                    ISBAttr_Surjective -> surjectiveCons
                    ISBAttr_Bijective  -> \ fresh -> injectiveCons fresh ++ surjectiveCons fresh

            cardinality <- domainSizeOf innerDomainFr

            let sizeCons = case sizeAttr of
                    SizeAttrNone           -> []
                    SizeAttrSize x         -> [ make opEq  x cardinality ]
                    SizeAttrMinSize x      -> [ make opLeq x cardinality ]
                    SizeAttrMaxSize y      -> [ make opGeq y cardinality ]
                    SizeAttrMinMaxSize x y -> [ make opLeq x cardinality
                                              , make opGeq y cardinality ]                    

            return $ Just $ \ fresh -> jectivityCons fresh ++ sizeCons

        structuralCons _ = fail "N/A {structuralCons}"

        -- downC (name, DomainSet "Explicit" (SetAttrSize size) innerDomain, ConstantSet constants) =
        --     let outIndexDomain = DomainInt [RangeBounded (ConstantInt 1) size]
        --     in  return $ Just
        --             [ ( outName name
        --               , DomainMatrix   outIndexDomain innerDomain
        --               , ConstantMatrix outIndexDomain constants
        --               ) ]
        downC _ = fail "N/A {downC}"

        -- up ctxt (name, domain@(DomainSet "Explicit" (SetAttrSize size) innerDomain)) =
        --     case lookup (outName name) ctxt of
        --         Nothing -> fail $ vcat $
        --             [ "No value for:" <+> pretty (outName name)
        --             , "When working on:" <+> pretty name
        --             , "With domain:" <+> pretty domain
        --             ] ++
        --             ("Bindings in context:" : prettyContext ctxt)
        --         Just constant ->
        --             case constant of
        --                 ConstantMatrix _ vals ->
        --                     return (name, ConstantSet vals)
        --                 _ -> fail $ vcat
        --                         [ "Expecting a matrix literal for:" <+> pretty (outName name)
        --                         , "But got:" <+> pretty constant
        --                         , "When working on:" <+> pretty name
        --                         , "With domain:" <+> pretty (DomainSet "Explicit" (SetAttrSize size) innerDomain)
        --                         ]
        up _ _ = fail "N/A {up}"

