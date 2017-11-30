{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.MSet.ExplicitWithFlags where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "mset-comprehension{ExplicitWithFlags}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, s), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} s) -> return (pat, s)
            _ -> na "rule_Comprehension"
        TypeMSet{}             <- typeOf s
        MSet_ExplicitWithFlags <- representationOf s
        [flags, values]        <- downX1 s
        DomainMatrix index _   <- domainOf values
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for mset-comprehension, ExplicitWithFlags representation"
            , do
                (jPat, j) <- quantifiedVar
                let val = [essence| &values[&j] |]
                return $ Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenDomainNoRepr jPat index)
                           , Condition [essence| &flags[&j] > 0 |]
                           ]
                        ++ transformBi (upd val) gocAfter
               )
    theRule _ = na "rule_Comprehension"


rule_Freq :: Rule
rule_Freq = "mset-freq{ExplicitWithFlags}" `namedRule` theRule where
    theRule p = do
        (mset, x)              <- match opFreq p
        TypeMSet{}             <- typeOf mset
        MSet_ExplicitWithFlags <- representationOf mset
        [flags, values]        <- downX1 mset
        DomainMatrix index _   <- domainOf values
        return
            ( "Vertical rule for mset-freq, ExplicitWithFlags representation"
            , do
                (iPat, i) <- quantifiedVar
                return
                    [essence|
                        sum([ &flags[&i]
                            | &iPat : &index
                            , &values[&i] = &x
                            ])
                    |]
            )


rule_frameUpdate :: Rule
rule_frameUpdate = "mset-frameUpdate{ExplicitWithFlags}" `namedRule` theRule where
    theRule p = do
        (source, target, sourceFocus, targetFocus, cons) <- match opFrameUpdate p

        TypeMSet{}                  <- typeOf source
        MSet_ExplicitWithFlags      <- representationOf source
        [sourceFlags, sourceValues] <- downX1 source
        (sourceIndex:_)             <- indexDomainsOf sourceValues

        TypeMSet{}                  <- typeOf target
        MSet_ExplicitWithFlags      <- representationOf target
        [targetFlags, targetValues] <- downX1 target
        (targetIndex:_)             <- indexDomainsOf targetValues

        return
            ( "Vertical rule for mset-frameUpdate, ExplicitWithFlags representation"
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
                                , make opAnd     (fromList [ [essence| &sourceFlags[&auxVar] > 0 |]
                                                           | (_,_,auxVar,_) <- sourceFocusNames ])

                                , make opAllDiff targetFocusVars
                                , make opAnd     (fromList [ [essence| &targetFlags[&auxVar] > 0 |]
                                                           | (_,_,auxVar,_) <- targetFocusNames ])

                                , consOut
                                , frameUpdateOut
                                , impliedSize
                                ]
                            ])

                return out
            )

