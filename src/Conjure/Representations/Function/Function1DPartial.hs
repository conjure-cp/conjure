{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Function.Function1DPartial ( function1DPartial ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.TH
import Conjure.Language.TypeOf
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainCanIndexMatrix, toIntDomain )


function1DPartial :: MonadFail m => Representation m
function1DPartial = Representation chck downD structuralCons downC up

    where

        chck f (DomainFunction _
                    attrs@(FunctionAttr _ FunctionAttr_Partial _)
                    innerDomainFr
                    innerDomainTo) | domainCanIndexMatrix innerDomainFr =
            DomainFunction "Function1DPartial" attrs
                <$> f innerDomainFr
                <*> f innerDomainTo
        chck _ _ = []

        nameFlags  name = mconcat [name, "_", "Function1DPartial_Flags"]
        nameValues name = mconcat [name, "_", "Function1DPartial_Values"]

        downD (name, DomainFunction "Function1DPartial"
                    (FunctionAttr _ FunctionAttr_Partial _)
                    innerDomainFr'
                    innerDomainTo) | domainCanIndexMatrix innerDomainFr' = do
            innerDomainFr <- toIntDomain innerDomainFr'
            return $ Just
                [ ( nameFlags name
                  , DomainMatrix
                      (forgetRepr innerDomainFr)
                      DomainBool
                  )
                , ( nameValues name
                  , DomainMatrix
                      (forgetRepr innerDomainFr)
                      innerDomainTo
                  )
                ]
        downD _ = fail "N/A {downD}"

        structuralCons (name, domain@(DomainFunction "Function1DPartial"
                    (FunctionAttr sizeAttr FunctionAttr_Partial jectivityAttr)
                    innerDomainFr'
                    innerDomainTo)) | domainCanIndexMatrix innerDomainFr' = do
            [flags,values]  <- rDownX function1DPartial name domain
            innerDomainFr   <- toIntDomain innerDomainFr'
            innerDomainFrTy <- typeOf innerDomainFr
            innerDomainToTy <- typeOf innerDomainTo

            let injectiveCons fresh = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0) innerDomainFrTy
                        (jPat, j) = quantifiedVar (fresh `at` 1) innerDomainToTy
                    in
                        [essence|
                            forAll &iPat : &innerDomainFr .
                                forAll &jPat : &innerDomainTo .
                                    &flags[&i] /\ &flags[&j] -> &values[&i] != &values[&j]
                        |]

            let surjectiveCons fresh = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0) innerDomainToTy
                        (jPat, j) = quantifiedVar (fresh `at` 1) innerDomainFrTy
                    in
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat : &innerDomainFr .
                                    &flags[&j] /\ &values[&j] = &i
                        |]

            let jectivityCons = case jectivityAttr of
                    ISBAttr_None       -> const []
                    ISBAttr_Injective  -> injectiveCons
                    ISBAttr_Surjective -> surjectiveCons
                    ISBAttr_Bijective  -> \ fresh -> injectiveCons fresh ++ surjectiveCons fresh

            let cardinality fresh =
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0) innerDomainFrTy
                    in
                        [essence| sum &iPat : &innerDomainFr . toInt(&flags[&i]) |]

            return $ Just $ \ fresh -> jectivityCons fresh ++ mkSizeCons sizeAttr (cardinality fresh)

        structuralCons _ = fail "N/A {structuralCons}"

        -- downC (name, DomainSet "Explicit" (SetAttrSize size) innerDomain, ConstantSet constants) =
        --     let outIndexDomain = DomainInt [RangeBounded (ConstantInt 1) size]
        --     in  return $ Just
        --             [ ( outName name
        --               , DomainMatrix   outIndexDomain innerDomain
        --               , ConstantMatrix outIndexDomain constants
        --               ) ]
        downC _ = fail "N/A {downC}"

        -- up ctxt (name, domain@(DomainFunction "Function1DPartial"
        --                         (FunctionAttr _ FunctionAttr_Partial _)
        --                         innerDomainFr _)) =
        --     case (lookup (nameFlags name) ctxt, lookup (nameValues name) ctxt) of
        --         (Just flagMatrix, Just valuesMatrix)
        --         Nothing -> fail $ vcat $
        --             [ "No value for:" <+> pretty (outName name)
        --             , "When working on:" <+> pretty name
        --             , "With domain:" <+> pretty domain
        --             ] ++
        --             ("Bindings in context:" : prettyContext ctxt)
        --         Just constant ->
        --             case constant of
        --                 ConstantMatrix _ vals -> do
        --                     froms <- domainValues innerDomainFr
        --                     return ( name
        --                            , ConstantFunction (zip froms vals)
        --                            )
        --                 _ -> fail $ vcat
        --                         [ "Expecting a matrix literal for:" <+> pretty (outName name)
        --                         , "But got:" <+> pretty constant
        --                         , "When working on:" <+> pretty name
        --                         , "With domain:" <+> pretty domain
        --                         ]
        up _ _ = fail "N/A {up}"

