{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Function.FunctionNDPartial ( functionNDPartial ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.TH
import Conjure.Language.TypeOf
import Conjure.Language.ZeroVal ( zeroVal )
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainCanIndexMatrix, domainValues, toIntDomain )


functionNDPartial :: MonadFail m => Representation m
functionNDPartial = Representation chck downD structuralCons downC up

    where

        chck f (DomainFunction _
                    attrs@(FunctionAttr _ FunctionAttr_Partial _)
                    innerDomainFr@(DomainTuple innerDomainFrs)
                    innerDomainTo) | all domainCanIndexMatrix innerDomainFrs =
            DomainFunction "FunctionNDPartial" attrs
                <$> f innerDomainFr
                <*> f innerDomainTo
        chck _ _ = []

        nameFlags  name = mconcat [name, "_", "FunctionNDPartial_Flags"]
        nameValues name = mconcat [name, "_", "FunctionNDPartial_Values"]

        downD (name, DomainFunction "FunctionNDPartial"
                    (FunctionAttr _ FunctionAttr_Partial _)
                    (DomainTuple innerDomainFrs')
                    innerDomainTo) | all domainCanIndexMatrix innerDomainFrs' = do
            innerDomainFrs <- mapM toIntDomain innerDomainFrs'
            let unroll []     j = j
                unroll (i:is) j = DomainMatrix i (unroll is j)
            return $ Just
                [ ( nameFlags name
                  , unroll (map forgetRepr innerDomainFrs) DomainBool
                  )
                , ( nameValues name
                  , unroll (map forgetRepr innerDomainFrs) innerDomainTo
                  )
                ]
        downD _ = fail "N/A {downD}"

        -- FIX
        structuralCons _ _
            (DomainFunction "FunctionNDPartial"
                (FunctionAttr sizeAttr FunctionAttr_Partial jectivityAttr)
                (DomainTuple innerDomainFrs')
                innerDomainTo) | all domainCanIndexMatrix innerDomainFrs' = do
            innerDomainFrs    <- mapM toIntDomain innerDomainFrs'
            let innerDomainFr =  DomainTuple innerDomainFrs
            innerDomainFrTy   <- typeOf innerDomainFr
            innerDomainToTy   <- typeOf innerDomainTo

            let injectiveCons fresh flags values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0) innerDomainFrTy
                        (jPat, j) = quantifiedVar (fresh `at` 1) innerDomainToTy
                    in
                        [essence|
                            forAll &iPat : &innerDomainFr .
                                forAll &jPat : &innerDomainTo .
                                    &flags[&i] /\ &flags[&j] -> &values[&i] != &values[&j]
                        |]

            let surjectiveCons fresh flags values = return $ -- list
                    let
                        (iPat, i) = quantifiedVar (fresh `at` 0) innerDomainToTy
                        (jPat, j) = quantifiedVar (fresh `at` 1) innerDomainFrTy
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
                        (iPat, i) = quantifiedVar (fresh `at` 0) innerDomainFrTy
                    in
                        [essence| sum &iPat : &innerDomainFr . toInt(&flags[&i]) |]

            return $ \ fresh refs ->
                case refs of
                    [flags,values] -> return $ concat [ jectivityCons fresh flags values
                                                      , mkSizeCons sizeAttr (cardinality fresh flags)
                                                      ]
                    _ -> fail "N/A {structuralCons} FunctionNDPartial"

        structuralCons _ _ _ = fail "N/A {structuralCons} FunctionNDPartial"

        downC ( name
              , DomainFunction "FunctionNDPartial"
                    (FunctionAttr _ FunctionAttr_Partial _)
                    innerDomainFr
                    innerDomainTo
              , ConstantFunction vals
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
                  , DomainMatrix
                      (forgetRepr innerDomainFrInt)
                      DomainBool
                  , ConstantMatrix (forgetRepr innerDomainFrInt) flagsOut
                  )
                , ( nameValues name
                  , DomainMatrix
                      (forgetRepr innerDomainFrInt)
                      innerDomainTo
                  , ConstantMatrix (forgetRepr innerDomainFrInt) valsOut
                  )
                ]
        downC _ = fail "N/A {downC}"

        up ctxt (name, domain@(DomainFunction "FunctionNDPartial"
                                (FunctionAttr _ FunctionAttr_Partial _)
                                (DomainTuple innerDomainFrs') _)) = do

            innerDomainFrs <- fmap (fmap e2c) <$> mapM toIntDomain (fmap (fmap Constant) innerDomainFrs')

            case (lookup (nameFlags name) ctxt, lookup (nameValues name) ctxt) of
                (Just flagMatrix, Just valuesMatrix) -> do
                    let
                        allIndices :: (MonadFail m, Pretty r) => [Domain r Constant] -> m [[Constant]]
                        allIndices = fmap sequence . mapM domainValues

                        index :: MonadFail m => Constant -> [Constant] -> m Constant
                        index m [] = return m
                        index (ConstantMatrix indexDomain vals) (i:is) = do
                            froms <- domainValues indexDomain
                            case lookup i (zip froms vals) of
                                Nothing -> fail "Value not found. FunctionNDPartial.up.index"
                                Just v  -> index v is
                        index m is = bug ("RelationAsMatrix.up.index" <+> pretty m <+> pretty (show is))

                    indices' <- allIndices innerDomainFrs'
                    indices  <- allIndices innerDomainFrs
                    vals     <- forM (zip indices indices') $ \ (these, these') -> do
                        flag  <- index flagMatrix   these
                        value <- index valuesMatrix these
                        case flag of
                            ConstantBool False -> return Nothing
                            ConstantBool True  -> return (Just (ConstantTuple these', value))
                            _ -> fail $ vcat
                                [ "Expecting a boolean literal, but got:" <+> pretty flag
                                , "                           , and    :" <+> pretty value
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                    return ( name
                           , ConstantFunction (catMaybes vals)
                           )

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
        up _ _ = fail "N/A {up}"

