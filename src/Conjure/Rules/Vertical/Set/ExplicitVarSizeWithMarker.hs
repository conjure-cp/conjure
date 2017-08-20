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


rule_frameUpdate :: Rule
rule_frameUpdate = "set-frameUpdate{ExplicitVarSizeWithMarker}" `namedRule` theRule where
    theRule p = do
        (old, new, names, cons) <- match opFrameUpdate p

        TypeSet{}                     <- typeOf old
        Set_ExplicitVarSizeWithMarker <- representationOf old
        [oldMarker, oldValues]        <- downX1 old
        (oldIndex:_)                  <- indexDomainsOf oldValues

        TypeSet{}                     <- typeOf new
        Set_ExplicitVarSizeWithMarker <- representationOf new
        [newMarker, newValues]        <- downX1 new
        (newIndex:_)                  <- indexDomainsOf newValues

        return
            ( "Vertical rule for frameUpdate, ExplicitVarSizeWithMarker representation"
            , do

                focusNames_a <- forM names $ \ (a,_) -> do
                    (auxName, aux) <- auxiliaryVar
                    return (a, auxName, aux, oldIndex)
                focusNames_b <- forM names $ \ (_,b) -> do
                    (auxName, aux) <- auxiliaryVar
                    return (b, auxName, aux, newIndex)

                let consOut = flip transform cons $ \ h -> case h of
                        Reference nm (Just FrameUpdateVar) ->
                            case ( [auxVar | (userName, _, auxVar, _) <- focusNames_a, userName == nm]
                                 , [auxVar | (userName, _, auxVar, _) <- focusNames_b, userName == nm] ) of
                                ([auxVar], _) -> [essence| &oldValues[&auxVar] |]
                                (_, [auxVar]) -> [essence| &newValues[&auxVar] |]
                                _             -> h
                        _ -> h

                (kPat, k) <- quantifiedVar
                (targetLPat, targetL) <- auxiliaryVar
                (targetMPat, targetM) <- auxiliaryVar

                -- keep everything out of focus unchanged
                let freezeFrame =
                        let
                            is_a t = make opOr  $ fromList [ [essence| &t = &i |]
                                                           | (_, _, i, _) <- focusNames_a
                                                           ]

                            k_is_b = make opOr  $ fromList [ [essence| &k = &i |]
                                                           | (_, _, i, _) <- focusNames_b
                                                           ]
                            k_gt_b = make opSum $ fromList [ [essence| toInt(&k >= &i) |]
                                                           | (_, _, i, _) <- focusNames_b
                                                           ]
                            l_gt_a = make opSum $ fromList [ [essence| toInt(&targetL >= &i) |]
                                                           | (_, _, i, _) <- focusNames_a
                                                           ]

                            targetAdjust = make opSum $ fromList
                                [ [essence| toInt(&condition) |]
                                | i <- [0 .. genericLength names - 1]
                                , let condition = make opAnd $ fromList
                                                    [ is_a [essence| &targetM + &jE |]
                                                    | j <- [0 .. i]
                                                    , let jE = Constant (ConstantInt j)
                                                    ]
                                ]
                            

                        in
                            [essence|
                                and([ &newValues[&k] = &oldValues[&targetM + &targetAdjust]
                                      /\ &k <= &newMarker
                                      /\ &targetM + &targetAdjust <= &oldMarker
                                    | &kPat : &newIndex
                                    , ! &k_is_b
                                    , letting &targetLPat be &k       - &k_gt_b
                                    , letting &targetMPat be &targetL + &l_gt_a
                                    ])
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
                            [ SuchThat
                                [ make opAllDiff (fromList [auxVar | (_,_,auxVar,_) <- focusNames_a])
                                , make opAnd     (fromList [ [essence| &auxVar <= &oldMarker |]
                                                           | (_,_,auxVar,_) <- focusNames_a ])

                                , make opAllDiff (fromList [auxVar | (_,_,auxVar,_) <- focusNames_b])
                                , make opAnd     (fromList [ [essence| &auxVar <= &newMarker |]
                                                           | (_,_,auxVar,_) <- focusNames_b ])

                                , consOut
                                , freezeFrame
                                ]
                            ])

                return out
            )
