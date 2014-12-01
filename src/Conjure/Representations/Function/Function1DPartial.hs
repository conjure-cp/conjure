{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Function.Function1DPartial ( function1DPartial ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.TH
import Conjure.Language.ZeroVal ( zeroVal )
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainValues, toIntDomain )


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
        downD _ = na "{downD} Function1DPartial"

        structuralCons f downX1
            (DomainFunction "Function1DPartial"
                (FunctionAttr sizeAttr FunctionAttr_Partial jectivityAttr)
                innerDomainFr'
                innerDomainTo) | domainCanIndexMatrix innerDomainFr' = do
            innerDomainFr   <- toIntDomain innerDomainFr'

            let injectiveCons fresh flags values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)
                    in
                        [essence|
                            forAll &iPat : &innerDomainFr .
                                forAll &jPat : &innerDomainTo .
                                    &flags[&i] /\ &flags[&j] -> &values[&i] != &values[&j]
                        |]

            let surjectiveCons fresh flags values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)
                    in
                        [essence|
                            forAll &iPat : &innerDomainTo .
                                exists &jPat : &innerDomainFr .
                                    &flags[&j] /\ &values[&j] = &i
                        |]

            let jectivityCons fresh flags values = case jectivityAttr of
                    ISBAttr_None       -> []
                    ISBAttr_Injective  -> injectiveCons  fresh flags values
                    ISBAttr_Surjective -> surjectiveCons fresh flags values
                    ISBAttr_Bijective  -> injectiveCons  fresh flags values
                                       ++ surjectiveCons fresh flags values

            let cardinality fresh flags =
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                    in
                        [essence| sum &iPat : &innerDomainFr . toInt(&flags[&i]) |]

            let dontCareInactives fresh flags values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0)
                    in
                        [essence|
                            forAll &iPat : &innerDomainFr . &flags[&i] = false ->
                                dontCare(&values[&i])
                        |]

            let innerStructuralCons fresh flags values = do
                    let (iPat, i) = quantifiedVar (headInf fresh)
                    let activeZone b = [essence| forAll &iPat : &innerDomainFr . &flags[&i] -> &b |]

                    -- preparing structural constraints for the inner guys
                    innerStructuralConsGen <- f innerDomainTo

                    let inLoop = [essence| &values[&i] |]
                    refs <- downX1 inLoop
                    outs <- innerStructuralConsGen (tail fresh) refs
                    return (map activeZone outs)

            return $ \ fresh refs ->
                case refs of
                    [flags,values] -> do
                        isc <- innerStructuralCons fresh flags values
                        return $ concat
                            [ jectivityCons     fresh flags values
                            , dontCareInactives fresh flags values
                            , mkSizeCons sizeAttr (cardinality fresh flags)
                            , isc
                            ]
                    _ -> na "{structuralCons} Function1DPartial"

        structuralCons _ _ _ = na "{structuralCons} Function1DPartial"

        downC ( name
              , DomainFunction "Function1DPartial"
                    (FunctionAttr _ FunctionAttr_Partial _)
                    innerDomainFr
                    innerDomainTo
              , ConstantAbstract (AbsLitFunction vals)
              ) | domainCanIndexMatrix innerDomainFr = do
            z <- zeroVal innerDomainTo
            innerDomainFrInt    <- fmap e2c <$> toIntDomain (fmap Constant innerDomainFr)
            froms               <- domainValues innerDomainFr
            (flagsOut, valsOut) <- unzip <$> sequence
                [ val
                | fr <- froms
                , let val = case lookup fr vals of
                                Nothing -> return (ConstantBool False, z)
                                Just v  -> return (ConstantBool True , v)
                ]
            return $ Just
                [ ( nameFlags name
                  , DomainMatrix (forgetRepr innerDomainFrInt) DomainBool
                  , ConstantAbstract $ AbsLitMatrix (forgetRepr innerDomainFrInt) flagsOut
                  )
                , ( nameValues name
                  , DomainMatrix (forgetRepr innerDomainFrInt) innerDomainTo
                  , ConstantAbstract $ AbsLitMatrix (forgetRepr innerDomainFrInt) valsOut
                  )
                ]
        downC _ = na "{downC} Function1DPartial"

        up ctxt (name, domain@(DomainFunction "Function1DPartial"
                                (FunctionAttr _ FunctionAttr_Partial _)
                                innerDomainFr _)) =
            case (lookup (nameFlags name) ctxt, lookup (nameValues name) ctxt) of
                ( Just (ConstantAbstract (AbsLitMatrix _ flagMatrix)) ,
                  Just (ConstantAbstract (AbsLitMatrix _ valuesMatrix)) ) -> do
                    froms          <- domainValues innerDomainFr
                    functionValues <- forM (zip3 flagMatrix froms valuesMatrix) $ \ (flag, from, to) ->
                        case flag of
                            ConstantBool b -> return $ if b then Just (from,to) else Nothing
                            _ -> fail $ vcat [ "Expected a boolean, but got:" <+> pretty flag
                                             , "When working on:" <+> pretty name
                                             , "With domain:" <+> pretty domain
                                             ]
                    return ( name, ConstantAbstract $ AbsLitFunction $ catMaybes functionValues )
                (Nothing, _) -> fail $ vcat $
                    [ "No value for:" <+> pretty (nameFlags name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> fail $ vcat $
                    [ "No value for:" <+> pretty (nameValues name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                _ -> fail $ vcat $
                    [ "Expected matrix literals for:" <+> pretty (nameFlags name)
                                            <+> "and" <+> pretty (nameValues name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        up _ _ = na "{up} Function1DPartial"

