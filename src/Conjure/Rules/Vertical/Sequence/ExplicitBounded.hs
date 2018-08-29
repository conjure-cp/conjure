{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Sequence.ExplicitBounded where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "sequence-comprehension{ExplicitBounded}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, sequ), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension"
        Sequence_ExplicitBounded <- representationOf sequ
        TypeSequence{}           <- typeOf sequ
        DomainSequence _ (SequenceAttr sizeAttr _) _ <- domainOf sequ
        maxSize <- case sizeAttr of
                    SizeAttr_Size x -> return x
                    SizeAttr_MaxSize x -> return x
                    SizeAttr_MinMaxSize _ x -> return x
                    _ -> fail "rule_Comprehension_Defined maxSize"
        [sLength, sValues]         <- downX1 sequ
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over a sequence, ExplicitBounded representation"
            , do
                (iPat, i) <- quantifiedVar
                let val = [essence| (&i, &sValues[&i]) |]
                return $ Comprehension
                       (upd val body)
                       $  gocBefore
                       ++ [ Generator (GenDomainNoRepr iPat (mkDomainIntB 1 maxSize))
                          , Condition [essence| &i <= &sLength |]
                          ]
                       ++ transformBi (upd val) gocAfter
               )
    theRule _ = na "rule_Comprehension"


rule_Card :: Rule
rule_Card = "sequence-cardinality{ExplicitBounded}" `namedRule` theRule where
    theRule [essence| |&s| |] = do
        TypeSequence{}           <- typeOf s
        Sequence_ExplicitBounded <- representationOf s
        [sLength, _]             <- downX1 s
        return
            ( "Vertical rule for sequence cardinality."
            , return sLength
            )
    theRule _ = na "rule_Card"


rule_Image_NotABool :: Rule
rule_Image_NotABool = "sequence-image{ExplicitBounded}-not-a-bool" `namedRule` theRule where
    theRule [essence| image(&sequ,&x) |] = do
        Sequence_ExplicitBounded <- representationOf sequ
        TypeSequence tyTo        <- typeOf sequ
        case tyTo of
            TypeBool -> na "sequence of bool"
            _        -> return ()
        [sLength,sValues] <- downX1 sequ
        return
            ( "Sequence image, ExplicitBounded representation, not-a-bool"
            , return [essence| { &sValues[&x]
                               @ such that &x <= &sLength
                               }
                             |]
            )
    theRule _ = na "rule_Image_NotABool"


rule_Image_Bool :: Rule
rule_Image_Bool = "sequence-image{ExplicitBounded}-bool" `namedRule` theRule where
    theRule p = do
        let
            imageChild ch@[essence| image(&sequ,&x) |] = do
                Sequence_ExplicitBounded <- representationOf sequ
                TypeSequence tyTo        <- typeOf sequ
                case tyTo of
                    TypeBool -> do
                        [sLength,sValues] <- downX1 sequ
                        tell $ return [essence| &x <= &sLength |]
                        return [essence| &sValues[&x] |]
                    _ -> return ch
            imageChild ch = return ch
        (p', flags) <- runWriterT (descendM imageChild p)
        case flags of
            [] -> na "rule_Image_Bool"
            _  -> do
                let flagsCombined = make opAnd $ fromList flags
                return
                    ( "Sequence image, ExplicitBounded representation, bool"
                    , return [essence| { &p' @ such that &flagsCombined } |]
                    )

rule_Leq :: Rule
rule_Leq = "sequence-leq{ExplicitBounded}" `namedRule` theRule where
    theRule [essence| &a ~<= &b |] = do
        TypeSequence aInnerTy    <- typeOf a
        TypeSequence bInnerTy    <- typeOf b
        unless (typeCanIndexMatrix aInnerTy && typeCanIndexMatrix bInnerTy) $ na "rule_Leq"
        Sequence_ExplicitBounded <- representationOf a
        Sequence_ExplicitBounded <- representationOf b
        [aLength, aValues]       <- downX1 a
        [bLength, bValues]       <- downX1 b
        return
            ( "Mapping over a sequence, ExplicitBounded representation"
            , return [essence|
                flatten([&aValues, [&aLength]]) <=lex
                flatten([&bValues, [&bLength]])
                             |]
               )
    theRule _ = na "rule_Comprehension"


rule_Lt :: Rule
rule_Lt = "sequence-lt{ExplicitBounded}" `namedRule` theRule where
    theRule [essence| &a ~< &b |] = do
        TypeSequence aInnerTy    <- typeOf a
        TypeSequence bInnerTy    <- typeOf b
        unless (typeCanIndexMatrix aInnerTy && typeCanIndexMatrix bInnerTy) $ na "rule_Lt"
        Sequence_ExplicitBounded <- representationOf a
        Sequence_ExplicitBounded <- representationOf b
        [aLength, aValues]       <- downX1 a
        [bLength, bValues]       <- downX1 b
        return
            ( "Mapping over a sequence, ExplicitBounded representation"
            , return [essence|
                flatten([&aValues, [&aLength]]) <lex
                flatten([&bValues, [&bLength]])
                             |]
               )
    theRule _ = na "rule_Comprehension"

