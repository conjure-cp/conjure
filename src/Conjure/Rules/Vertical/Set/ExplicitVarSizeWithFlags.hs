{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Set.ExplicitVarSizeWithFlags where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "set-comprehension{ExplicitVarSizeWithFlags}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, s), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} s) -> return (pat, matchDefs [opToSet,opToMSet] s)
            _ -> na "rule_Comprehension"
        TypeSet{}                    <- typeOf s
        Set_ExplicitVarSizeWithFlags <- representationOf s
        [flags, values]              <- downX1 s
        DomainMatrix index _         <- domainOf values
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for set-comprehension, ExplicitVarSizeWithFlags representation"
            , do
                (jPat, j) <- quantifiedVar
                let val = [essence| &values[&j] |]
                return $ Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenDomainNoRepr jPat index)
                           , Condition [essence| &flags[&j] |]
                           ]
                        ++ transformBi (upd val) gocAfter
               )
    theRule _ = na "rule_Comprehension"


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "set-powerSet-comprehension{ExplicitVarSizeWithFlags}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (setPat, setPatNum, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr setPat@(AbsPatSet pats) expr) -> return (setPat, length pats, expr)
            _ -> na "rule_PowerSet_Comprehension"
        s                            <- match opPowerSet expr
        TypeSet{}                    <- typeOf s
        Set_ExplicitVarSizeWithFlags <- representationOf s
        [flags, values]              <- downX1 s
        DomainMatrix index _         <- domainOf values
        let upd val old = lambdaToFunction setPat old val
        return
            ( "Vertical rule for set-comprehension, ExplicitVarSizeWithFlags representation"
            , do
                outPats <- replicateM setPatNum quantifiedVar
                let val = AbstractLiteral $ AbsLitSet
                        [ [essence| &values[&j] |] | (_,j) <- outPats ]
                return $ Comprehension (upd val body) $ concat
                        [ gocBefore
                        , concat
                            [ [ Generator (GenDomainNoRepr pat index)
                              , Condition [essence| &flags[&patX] |]
                              ]
                            | (pat,patX) <- take 1 outPats
                            ]
                        , concat
                            [ [ Generator (GenDomainNoRepr pat index)
                              , Condition [essence| &patX > &beforeX |]
                              , Condition [essence| &flags[&patX] |]
                              ]
                            | ((_, beforeX), (pat, patX)) <- zip outPats (tail outPats)
                            ]
                        , transformBi (upd val) gocAfter
                        ]
            )
    theRule _ = na "rule_PowerSet_Comprehension"


rule_frameUpdate :: Rule
rule_frameUpdate = "set-frameUpdate{ExplicitVarSizeWithFlags}" `namedRule` theRule where
    theRule p = do
        (old, new, names, cons) <- match opFrameUpdate p

        TypeSet{}                    <- typeOf old
        Set_ExplicitVarSizeWithFlags <- representationOf old
        [oldFlags, oldValues]        <- downX1 old
        (oldIndex:_)                 <- indexDomainsOf oldValues

        TypeSet{}                    <- typeOf new
        Set_ExplicitVarSizeWithFlags <- representationOf new
        [newFlags, newValues]        <- downX1 new
        (newIndex:_)                 <- indexDomainsOf newValues

        return
            ( "Vertical rule for frameUpdate, ExplicitVarSizeWithFlags representation"
            , do

                focusNames_a <- forM names $ \ (a,_) -> do
                    (auxName, aux) <- auxiliaryVar
                    return (a, auxName, aux, oldIndex)

                focusNames_b <- forM names $ \ (_,b) -> do
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
                (contiguousCountsPat, contiguousCounts) <- auxiliaryVar
                (offsetsPat, offsets) <- auxiliaryVar

                -- build contiguousCounts
                let
                    contiguousCountsCons =
                        let
                            is_a t = make opOr  $ fromList [ [essence| &t = &i |]
                                                           | (_, _, i, _) <- focusNames_a
                                                           ]

                            maxOldIndex = [essence| max(`&oldIndex`) |]
                            maxOldIndex_is_a = is_a maxOldIndex

                            maxOldIndexMinusK_is_a = is_a [essence| &maxOldIndex - &k |]

                        in
                            [essence|
                                (&contiguousCounts[&maxOldIndex] = toInt(&maxOldIndex_is_a)) /\
                                forAll &kPat : int(1..&maxOldIndex-1) .
                                    (&contiguousCounts[&maxOldIndex-&k] = 0 <-> ! &maxOldIndexMinusK_is_a) /\
                                    (&maxOldIndexMinusK_is_a -> &contiguousCounts[&maxOldIndex-&k] = &contiguousCounts[&maxOldIndex-(&k-1)] +1)
                            |]

                -- build offsets matrix
                let
                    buildOffSetsCons = 
                        let
                            is_b t = make opOr  $ fromList [ [essence| &t = &i |]
                                                           | (_, _, i, _) <- focusNames_b
                                                           ]

                            k_is_b = is_b k

                            one_is_b = is_b [essence| 1 |]

                            maxOldIndex = [essence| max(`&oldIndex`) |]

                        in
                            [essence|
                                (&offsets[1] = (0 - toInt(&one_is_b) + catchUndef(&contiguousCounts[1 - toInt(&one_is_b)], 0))) /\
                                forAll &kPat : int(2..&maxOldIndex) .
                                    &offsets[&k] = (&offsets[&k-1] - toInt(&k_is_b))
                                                + catchUndef(&contiguousCounts[(&offsets[&k-1] - toInt(&k_is_b)) + &k], 0)
                            |]

                -- keep everything out of focus unchanged
                let freezeFrameCons =
                        let
                            k_is_b = make opOr  $ fromList [ [essence| &k = &i |]
                                                           | (_, _, i, _) <- focusNames_b
                                                           ]

                            maxOldIndex = [essence| max(`&oldIndex`) |]

                        in
                            [essence|
                                forAll &kPat : int(1..&maxOldIndex) .
                                    (! &k_is_b) -> &newValues[&k] = &oldValues[&k + &offsets[&k]]
                            |]

                let out = WithLocals
                        [essence| true |]
                        (AuxiliaryVars $
                            [ Declaration (FindOrGiven LocalFind auxName domain)
                            | (_userName, auxName, _auxVar, domain) <- focusNames_a
                            ] ++
                            [ Declaration (FindOrGiven LocalFind auxName domain)
                            | (_userName, auxName, _auxVar, domain) <- focusNames_b
                            ] ++
                            [ Declaration (FindOrGiven LocalFind contiguousCountsPat
                                    (DomainMatrix oldIndex
                                        (DomainInt [RangeBounded
                                                        (Constant $ ConstantInt 0)
                                                        (Constant $ ConstantInt $ genericLength focusNames_a)])))
                            , Declaration (FindOrGiven LocalFind offsetsPat
                                    (DomainMatrix oldIndex
                                        (DomainInt [RangeBounded
                                                        (Constant $ ConstantInt $ negate $ genericLength focusNames_a)
                                                        (Constant $ ConstantInt $          genericLength focusNames_a)])))
                            ] ++
                            [ SuchThat
                                [ make opAllDiff (fromList [auxVar | (_,_,auxVar,_) <- focusNames_a])
                                , make opAnd     (fromList [ [essence| &oldFlags[&auxVar] |]
                                                           | (_,_,auxVar,_) <- focusNames_a ])

                                , make opAllDiff (fromList [auxVar | (_,_,auxVar,_) <- focusNames_b])
                                , make opAnd     (fromList [ [essence| &newFlags[&auxVar] |]
                                                           | (_,_,auxVar,_) <- focusNames_b ])

                                , contiguousCountsCons
                                , buildOffSetsCons
                                , freezeFrameCons

                                , consOut
                                ]
                            ])

                return out
            )

