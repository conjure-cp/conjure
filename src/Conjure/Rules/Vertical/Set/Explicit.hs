{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Set.Explicit where

import Conjure.Rules.Import


rule_Comprehension :: Rule
rule_Comprehension = "set-comprehension{Explicit}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, s), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} s) -> return (pat, matchDefs [opToSet,opToMSet] s)
            _ -> na "rule_Comprehension"
        TypeSet{}            <- typeOf s
        Set_Explicit         <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let upd val old = lambdaToFunction pat old val
        return
            ( "Vertical rule for set-comprehension, Explicit representation"
            , do
                (jPat, j) <- quantifiedVar
                let val = [essence| &m[&j] |]
                return $ Comprehension (upd val body)
                        $  gocBefore
                        ++ [ Generator (GenDomainNoRepr jPat index) ]
                        ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension"


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "set-powerSet-comprehension{Explicit}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (setPat, setPatNum, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr setPat@(AbsPatSet pats) expr) -> return (setPat, length pats, expr)
            _ -> na "rule_PowerSet_Comprehension"
        s                    <- match opPowerSet expr
        TypeSet{}            <- typeOf s
        Set_Explicit         <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        let upd val old = lambdaToFunction setPat old val
        return
            ( "Vertical rule for set-comprehension, Explicit representation"
            , do
                outPats <- replicateM setPatNum quantifiedVar
                let val = AbstractLiteral $ AbsLitSet [ [essence| &m[&j] |] | (_,j) <- outPats ]
                return $ Comprehension (upd val body) $ concat
                        [ gocBefore
                        , concat
                            [ [ Generator (GenDomainNoRepr pat index) ]
                            | (pat,_) <- take 1 outPats
                            ]
                        , concat
                            [ [ Generator (GenDomainNoRepr pat index)
                              , Condition [essence| &patX > &beforeX |]
                              ]
                            | ((_, beforeX), (pat, patX)) <- zip outPats (tail outPats)
                            ]
                        , transformBi (upd val) gocAfter
                        ]
            )
    theRule _ = na "rule_PowerSet_Comprehension"


rule_Card :: Rule
rule_Card = "set-card{Explicit}" `namedRule` theRule where
    theRule p = do
        s                                         <- match opTwoBars p
        TypeSet{}                                 <- typeOf s
        Set_Explicit                              <- representationOf s
        DomainSet _ (SetAttr (SizeAttr_Size n)) _ <- domainOf s
        return
            ( "Vertical rule for set cardinality, Explicit representation."
            , return n
            )


-- | the first member
rule_Min :: Rule
rule_Min = "set-min{Explicit}" `namedRule` theRule where
    theRule p = do
        s                    <- match opMin p
        TypeSet{}            <- typeOf s
        Set_Explicit         <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        minInIndex           <-
            case index of
                DomainInt [RangeBounded lb _] -> return lb
                _ -> do
                    (jPat, j) <- quantifiedVar
                    return [essence| min([&j | &jPat : &index]) |]
        return
            ( "Vertical rule for set min, Explicit representation."
            , return [essence| &m[&minInIndex] |]
            )


-- | the last member
rule_Max :: Rule
rule_Max = "set-max{Explicit}" `namedRule` theRule where
    theRule p = do
        s                    <- match opMax p
        TypeSet{}            <- typeOf s
        Set_Explicit         <- representationOf s
        [m]                  <- downX1 s
        DomainMatrix index _ <- domainOf m
        maxInIndex           <-
            case index of
                DomainInt [RangeBounded _ ub] -> return ub
                _ -> do
                    (jPat, j) <- quantifiedVar
                    return [essence| max([&j | &jPat : &index]) |]
        return
            ( "Vertical rule for set max, Explicit representation."
            , return [essence| &m[&maxInIndex] |]
            )


-- | This is the specification of frameUpdate(old, new, [(x1,y1), (x2, y2)], cons)
-- { true
-- @
--     find x1, x2 : indexOld
--     find y1, y2 : indexNew
--     such that
--         allDiff([x1, x2]),
--         allDiff([y1, y2]),
--         cons(x1,x2,y1,y2),
--         and([ new[k] = &m + sum([ toInt(or([m in {x1,x2}])),
--                                 , toInt(or([m in {x1,x2}]) /\ or([m+1 in {x1,x2}]))
--                                 ])
--             | k : indexNew
--             , !(k in {y1,y2})
--             , letting l be k - sum(&k >= &y1, &k >= &y2)            $ number of empty places to the left
--             , letting m be l + sum(&l >= &x1, &l >= &x2)
--             ])
-- }
rule_frameUpdate :: Rule
rule_frameUpdate = "set-frameUpdate" `namedRule` theRule where
    theRule p = do
        (old, new, names, cons) <- match opFrameUpdate p

        TypeSet{}    <- typeOf old
        Set_Explicit <- representationOf old
        [oldM]       <- downX1 old
        (oldIndex:_) <- indexDomainsOf oldM

        TypeSet{}    <- typeOf new
        Set_Explicit <- representationOf new
        [newM]       <- downX1 new
        (newIndex:_) <- indexDomainsOf newM

        -- traceM $ show $ "old  :" <+> pretty old
        -- traceM $ show $ "new  :" <+> pretty new
        -- traceM $ show $ "names:" <+> pretty (show names)
        -- traceM $ show $ "cons :" <+> pretty cons

        return
            ( "Vertical rule for frameUpdate, Explicit representation"
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
                                ([auxVar], _) -> [essence| &oldM[&auxVar] |]
                                (_, [auxVar]) -> [essence| &newM[&auxVar] |]
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

