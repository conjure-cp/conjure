{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Set.Occurrence where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "set-comprehension{Occurrence}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, iPat, s), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@(Single iPat) s) -> return (pat, iPat, matchDefs [opToSet,opToMSet] s)
            _ -> na "rule_Comprehension"
        TypeSet{}            <- typeOf s
        Set_Occurrence       <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let i = Reference iPat Nothing
        return
            ( "Vertical rule for set-comprehension, Occurrence representation"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenDomainNoRepr pat index)
                       , Condition [essence| &m[&i] |]
                       ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Comprehension"


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "set-powerSet-comprehension{Occurrence}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pats, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr (AbsPatSet pats) expr) -> return (pats, expr)
            _ -> na "rule_PowerSet_Comprehension"
        -- assert pats are Single{}
        patNames <- forM pats $ \ pat -> case pat of Single nm -> return nm
                                                     _ -> na "rule_PowerSet_Comprehension: pat not s Single"
        s                    <- match opPowerSet expr
        TypeSet{}            <- typeOf s
        Set_Occurrence       <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        return
            ( "Vertical rule for set-comprehension, Occurrence representation"
            , return $
                Comprehension body $ concat
                    [ gocBefore
                    , concat
                        [ [ Generator (GenDomainNoRepr (Single pat) index)
                          , Condition [essence| &m[&patX] |]
                          ]
                        | pat <- take 1 patNames
                        , let patX = Reference pat Nothing
                        ]
                    , concat
                        [ [ Generator (GenDomainNoRepr (Single pat) index)
                          , Condition [essence| &patX > &beforeX |]
                          , Condition [essence| &m[&patX] |]
                          ]
                        | (before, pat) <- zip patNames (tail patNames)
                        , let beforeX = Reference before Nothing
                        , let patX = Reference pat Nothing
                        ]
                    , gocAfter
                    ]
            )
    theRule _ = na "rule_PowerSet_Comprehension"


rule_In :: Rule
rule_In = "set-in{Occurrence}" `namedRule` theRule where
    theRule p = do
        (x, s)         <- match opIn p
        TypeSet{}      <- typeOf s
        Set_Occurrence <- representationOf s
        [m]            <- downX1 s
        return
            ( "Vertical rule for set-in, Occurrence representation"
            , return $ make opIndexing m x
            )


rule_frameUpdate :: Rule
rule_frameUpdate = "set-frameUpdate{Occurrence}" `namedRule` theRule where
    theRule p = do
        (source, target, sourceFocus, targetFocus, cons) <- match opFrameUpdate p

        TypeSet{}       <- typeOf source
        Set_Occurrence  <- representationOf source
        [sourceValues]  <- downX1 source
        (sourceIndex:_) <- indexDomainsOf sourceValues

        TypeSet{}       <- typeOf target
        Set_Occurrence  <- representationOf target
        [targetValues]  <- downX1 target
        (targetIndex:_) <- indexDomainsOf targetValues

        return
            ( "Vertical rule for set-frameUpdate, Occurrence representation"
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
                                ([auxVar], _) -> auxVar
                                (_, [auxVar]) -> auxVar
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
                                , make opAnd     (fromList [ [essence| &sourceValues[&auxVar] |]
                                                           | (_,_,auxVar,_) <- sourceFocusNames ])

                                , make opAllDiff targetFocusVars
                                , make opAnd     (fromList [ [essence| &targetValues[&auxVar] |]
                                                           | (_,_,auxVar,_) <- targetFocusNames ])

                                , consOut
                                , frameUpdateOut
                                , impliedSize
                                ]
                            ])

                return out
            )

