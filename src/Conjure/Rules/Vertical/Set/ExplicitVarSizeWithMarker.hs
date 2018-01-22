{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Set.ExplicitVarSizeWithMarker where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "set-comprehension{ExplicitVarSizeWithMarker}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, s), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} s) -> return (pat, matchDefs [opToSet,opToMSet] s)
            _ -> na "rule_Comprehension"
        TypeSet{}                     <- typeOf s
        Set_ExplicitVarSizeWithMarker <- representationOf s
        [marker, values]              <- downX1 s
        DomainMatrix index _          <- domainOf values
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for set-comprehension, ExplicitVarSizeWithMarker representation"
            , do
                (jPat, j) <- quantifiedVar
                let val = [essence| &values[&j] |]
                return $ Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenDomainNoRepr jPat index)
                           , Condition [essence| &j <= &marker |]
                           ]
                        ++ transformBi (upd val) gocAfter
               )
    theRule _ = na "rule_Comprehension"


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "set-powerSet-comprehension{ExplicitVarSizeWithMarker}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (setPat, setPatNum, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr setPat@(AbsPatSet pats) expr) -> return (setPat, length pats, expr)
            _ -> na "rule_PowerSet_Comprehension"
        s                             <- match opPowerSet expr
        TypeSet{}                     <- typeOf s
        Set_ExplicitVarSizeWithMarker <- representationOf s
        [marker, values]              <- downX1 s
        DomainMatrix index _          <- domainOf values
        let upd val old = lambdaToFunction setPat old val
        return
            ( "Vertical rule for set-comprehension, ExplicitVarSizeWithMarker representation"
            , do
                outPats <- replicateM setPatNum quantifiedVar
                let val = AbstractLiteral $ AbsLitSet
                        [ [essence| &values[&j] |] | (_,j) <- outPats ]
                return $ Comprehension (upd val body) $ concat
                        [ gocBefore
                        , concat
                            [ [ Generator (GenDomainNoRepr pat index)
                              , Condition [essence| &patX <= &marker |]
                              ]
                            | (pat,patX) <- take 1 outPats
                            ]
                        , concat
                            [ [ Generator (GenDomainNoRepr pat index)
                              , Condition [essence| &patX > &beforeX |]
                              , Condition [essence| &patX <= &marker |]
                              ]
                            | ((_, beforeX), (pat, patX)) <- zip outPats (tail outPats)
                            ]
                        , transformBi (upd val) gocAfter
                        ]
            )
    theRule _ = na "rule_PowerSet_Comprehension"


rule_Card :: Rule
rule_Card = "set-card{ExplicitVarSizeWithMarker}" `namedRule` theRule where
    theRule p = do
        s                             <- match opTwoBars p
        TypeSet{}                     <- typeOf s
        Set_ExplicitVarSizeWithMarker <- representationOf s
        [marker, _values]             <- downX1 s
        return
            ( "Vertical rule for set cardinality, ExplicitVarSizeWithMarker representation."
            , return marker
            )


rule_frameUpdate_propagator :: Rule
rule_frameUpdate_propagator = "set-frameUpdate{ExplicitVarSizeWithMarker}" `namedRule` theRule where
    theRule p = do
        (source, target, sourceFocus, targetFocus, cons) <- match opFrameUpdate p

        TypeSet{}                     <- typeOf source
        Set_ExplicitVarSizeWithMarker <- representationOf source
        [sourceMarker, sourceValues]  <- downX1 source
        (sourceIndex:_)               <- indexDomainsOf sourceValues

        TypeSet{}                     <- typeOf target
        Set_ExplicitVarSizeWithMarker <- representationOf target
        [targetMarker, targetValues]  <- downX1 target
        (targetIndex:_)               <- indexDomainsOf targetValues

        return
            ( "Vertical rule for set-frameUpdate, ExplicitVarSizeWithMarker representation"
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

                    nbSources = Constant $ ConstantInt $ genericLength sourceFocus
                    nbTargets = Constant $ ConstantInt $ genericLength targetFocus
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
                                , make opAnd     (fromList [ [essence| &auxVar <= &sourceMarker |]
                                                           | (_,_,auxVar,_) <- sourceFocusNames ])

                                , make opAllDiff targetFocusVars
                                , make opAnd     (fromList [ [essence| &auxVar <= &targetMarker |]
                                                           | (_,_,auxVar,_) <- targetFocusNames ])

                                , consOut
                                , frameUpdateOut
                                , impliedSize
                                ]
                            ])

                return out
            )


rule_frameUpdate_decomposition :: Rule
rule_frameUpdate_decomposition = "set-frameUpdate{ExplicitVarSizeWithMarker}" `namedRule` theRule where
    theRule p = do
        (old, new, oldFocus, newFocus, cons) <- match opFrameUpdate p

        TypeSet{}                     <- typeOf old
        Set_ExplicitVarSizeWithMarker <- representationOf old
        [oldMarker, oldValues]        <- downX1 old
        (oldIndex:_)                  <- indexDomainsOf oldValues

        TypeSet{}                     <- typeOf new
        Set_ExplicitVarSizeWithMarker <- representationOf new
        [newMarker, newValues]        <- downX1 new
        (newIndex:_)                  <- indexDomainsOf newValues

        return
            ( "Vertical rule for set-frameUpdate, ExplicitVarSizeWithMarker representation"
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

                let nbOlds = Constant $ ConstantInt $ genericLength oldFocus
                let nbNews = Constant $ ConstantInt $ genericLength newFocus

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
                                    &k <= &oldMarker /\ (! &k_is_a) -> &newValues[&k] = &oldValues[&k]
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
                                , make opAnd     (fromList [ [essence| &auxVar <= &oldMarker |]
                                                           | (_,_,auxVar,_) <- focusNames_a ])

                                , make opAllDiff (fromList [auxVar | (_,_,auxVar,_) <- focusNames_b])
                                , make opAnd     (fromList [ [essence| &auxVar <= &newMarker |]
                                                           | (_,_,auxVar,_) <- focusNames_b ])

                                , freezeFrameCons

                                , consOut
                                , impliedSize
                                ] ++ focusSetsEqual
                            ])

                return out
            )

