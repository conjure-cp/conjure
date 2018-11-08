{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.MSet.ExplicitWithRepetition where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "mset-comprehension{ExplicitWithRepetition}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, s), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} s) -> return (pat, s)
            _ -> na "rule_Comprehension"
        TypeMSet{}                  <- typeOf s
        MSet_ExplicitWithRepetition <- representationOf s
        [flag, values]              <- downX1 s
        DomainMatrix index _        <- domainOf values
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for mset-comprehension, ExplicitWithRepetition representation"
            , do
                (jPat, j) <- quantifiedVar
                let val = [essence| &values[&j] |]
                return $ Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenDomainNoRepr jPat index)
                           , Condition [essence| &j <= &flag |]
                           ]
                        ++ transformBi (upd val) gocAfter
               )
    theRule _ = na "rule_Comprehension"


rule_frameUpdate_propagator :: Rule
rule_frameUpdate_propagator = "mset-frameUpdate{ExplicitWithRepetition}" `namedRule` theRule where
    theRule p = do
        (source, target, sourceFocus, targetFocus, cons) <- match opFrameUpdate p

        TypeMSet{}                  <- typeOf source
        MSet_ExplicitWithRepetition <- representationOf source
        [sourceFlag, sourceValues]  <- downX1 source
        (sourceIndex:_)             <- indexDomainsOf sourceValues

        TypeMSet{}                  <- typeOf target
        MSet_ExplicitWithRepetition <- representationOf target
        [targetFlag, targetValues]  <- downX1 target
        (targetIndex:_)             <- indexDomainsOf targetValues

        return
            ( "Vertical rule for set-frameUpdate, ExplicitWithRepetition representation"
            , do

                sourceFocusNames <- forM sourceFocus $ \ x -> do
                    (auxName, aux) <- auxiliaryVar
                    return (x, auxName, aux, sourceIndex)

                targetFocusNames <- forM targetFocus $ \ x -> do
                    (auxName, aux) <- auxiliaryVar
                    return (x, auxName, aux, targetIndex)

                let
                    sourceFocusVars :: Expression
                    sourceFocusVars = fromList $ [auxVar | (_,_,auxVar,_) <- sourceFocusNames]

                    targetFocusVars :: Expression
                    targetFocusVars = fromList $ [auxVar | (_,_,auxVar,_) <- targetFocusNames]

                    consOut :: Expression
                    consOut = flip transform cons $ \ h -> case h of
                        Reference nm (Just FrameUpdateVar{}) ->
                            case ( [auxVar | (userName, _, auxVar, _) <- sourceFocusNames, userName == nm]
                                 , [auxVar | (userName, _, auxVar, _) <- targetFocusNames, userName == nm] ) of
                                ([auxVar], _) -> [essence| &sourceValues[&auxVar] |]
                                (_, [auxVar]) -> [essence| &targetValues[&auxVar] |]
                                _             -> h
                        _ -> h

                    frameUpdateOut :: Expression
                    frameUpdateOut = [essence| frameUpdate(&sourceValues, &targetValues, &sourceFocusVars, &targetFocusVars) |]

                    nbSources = fromInt $ genericLength sourceFocus
                    nbTargets = fromInt $ genericLength targetFocus
                    impliedSize =
                        [essence| |&target| = |&source| + (&nbTargets - &nbSources) |]

                    out = WithLocals
                        [essence| true |]
                        (AuxiliaryVars $
                            [ Declaration (FindOrGiven LocalFind auxName domain)
                            | (_userName, auxName, _auxVar, domain) <- sourceFocusNames
                            ] ++
                            [ Declaration (FindOrGiven LocalFind auxName domain)
                            | (_userName, auxName, _auxVar, domain) <- targetFocusNames
                            ] ++
                            [ SuchThat
                                [ make opAllDiff sourceFocusVars
                                , make opAnd     (fromList [ [essence| &auxVar <= &sourceFlag |]
                                                           | (_,_,auxVar,_) <- sourceFocusNames ])

                                , make opAllDiff targetFocusVars
                                , make opAnd     (fromList [ [essence| &auxVar <= &targetFlag |]
                                                           | (_,_,auxVar,_) <- targetFocusNames ])

                                , consOut
                                , frameUpdateOut
                                , impliedSize
                                ]
                            ])

                return out
            )


rule_frameUpdate_decomposition :: Rule
rule_frameUpdate_decomposition = "mset-frameUpdate{ExplicitWithRepetition}" `namedRule` theRule where
    theRule p = do
        (old, new, oldFocus, newFocus, cons) <- match opFrameUpdate p

        TypeMSet{}                   <- typeOf old
        MSet_ExplicitWithRepetition  <- representationOf old
        [oldFlag, oldValues]         <- downX1 old
        (oldIndex:_)                 <- indexDomainsOf oldValues

        TypeMSet{}                   <- typeOf new
        MSet_ExplicitWithRepetition  <- representationOf new
        [newFlag, newValues]         <- downX1 new
        (newIndex:_)                 <- indexDomainsOf newValues

        return
            ( "Vertical rule for mset-frameUpdate, ExplicitWithRepetition representation"
            , do

                focusNames_a <- forM oldFocus $ \ a -> do
                    (auxName, aux) <- auxiliaryVar
                    return (a, auxName, aux, oldIndex)

                focusNames_b <- forM newFocus $ \ b -> do
                    (auxName, aux) <- auxiliaryVar
                    return (b, auxName, aux, newIndex)

                let consOut = flip transform cons $ \ h -> case h of
                        Reference nm (Just FrameUpdateVar{}) ->
                            case ( [auxVar | (userName, _, auxVar, _) <- focusNames_a, userName == nm]
                                 , [auxVar | (userName, _, auxVar, _) <- focusNames_b, userName == nm] ) of
                                ([auxVar], _) -> [essence| &oldValues[&auxVar] |]
                                (_, [auxVar]) -> [essence| &newValues[&auxVar] |]
                                _             -> h
                        _ -> h

                (kPat, k) <- quantifiedVar

                let nbOlds = fromInt $ genericLength oldFocus
                let nbNews = fromInt $ genericLength newFocus

                -- keep everything out of focus unchanged
                let freezeFrameCons =
                        let
                            k_is_a = make opOr  $ fromList [ [essence| &k = &i |]
                                                           | (_, _, i, _) <- focusNames_a
                                                           ]
                            maxOldIndex = [essence| max(`&oldIndex`) |]

                        in
                            [essence|
                                forAll &kPat : int(1..&maxOldIndex) .
                                    &k <= &oldFlag /\ (! &k_is_a) -> &newValues[&k] = &oldValues[&k]
                            |]

                -- assigns the smaller set to be a subset of the larger one
                let focusSetsEqual =
                        let
                            as = AbstractLiteral $ AbsLitSet [ i | (_, _, i, _) <- focusNames_a ]
                            bs = AbstractLiteral $ AbsLitSet [ i | (_, _, i, _) <- focusNames_b ]

                        in
                            concat
                                [ [ [essence| &as subsetEq &bs |] | length oldFocus <= length newFocus ]
                                , [ [essence| &as supsetEq &bs |] | length oldFocus >= length newFocus ]
                                ]

                let impliedSize =
                        [essence| |&new| = |&old| + (&nbNews - &nbOlds) |]

                let out = WithLocals
                        [essence| true |]
                        (AuxiliaryVars $
                            [ Declaration (FindOrGiven LocalFind auxName domain)
                            | (_userName, auxName, _auxVar, domain) <- focusNames_a
                            ] ++
                            [ Declaration (FindOrGiven LocalFind auxName domain)
                            | (_userName, auxName, _auxVar, domain) <- focusNames_b
                            ] ++
                            [ SuchThat $
                                [ make opAllDiff (fromList [auxVar | (_,_,auxVar,_) <- focusNames_a])
                                , make opAnd     (fromList [ [essence| &auxVar <= &oldFlag |]
                                                           | (_,_,auxVar,_) <- focusNames_a ])

                                , make opAllDiff (fromList [auxVar | (_,_,auxVar,_) <- focusNames_b])
                                , make opAnd     (fromList [ [essence| &auxVar <= &newFlag |]
                                                           | (_,_,auxVar,_) <- focusNames_b ])

                                , freezeFrameCons

                                , consOut
                                , impliedSize
                                ] ++ focusSetsEqual
                            ])

                return out
            )

