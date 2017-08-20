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
rule_frameUpdate = "set-frameUpdate" `namedRule` theRule where
    theRule p = do
        (old, new, names, cons) <- match opFrameUpdate p

        TypeSet{}      <- typeOf old
        Set_Occurrence <- representationOf old
        [oldM]         <- downX1 old
        (oldIndex:_)   <- indexDomainsOf oldM

        TypeSet{}      <- typeOf new
        Set_Occurrence <- representationOf new
        [newM]         <- downX1 new
        (newIndex:_)   <- indexDomainsOf newM

        -- traceM $ show $ "old  :" <+> pretty old
        -- traceM $ show $ "new  :" <+> pretty new
        -- traceM $ show $ "names:" <+> pretty (show names)
        -- traceM $ show $ "cons :" <+> pretty cons

        return
            ( "Vertical rule for frameUpdate, Occurrence representation"
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
                                ([auxVar], _) -> [essence| &auxVar |]
                                (_, [auxVar]) -> [essence| &auxVar |]
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
                            -- trace (show $ "rule_frameUpdate targetAdjust" <++> pretty targetAdjust) $
                            [essence|
                                and([ &newM[&k] = &oldM[&targetM + &targetAdjust]
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
                                , make opAllDiff (fromList [auxVar | (_,_,auxVar,_) <- focusNames_b])
                                , consOut
                                , freezeFrame
                                ]
                            ])
                -- traceM $ show $ "rule_frameUpdate consOut     " <++> pretty consOut
                -- traceM $ show $ "rule_frameUpdate freezeFrame " <++> pretty freezeFrame
                -- traceM $ show $ "rule_frameUpdate out         " <++> pretty out
                return out
            )

