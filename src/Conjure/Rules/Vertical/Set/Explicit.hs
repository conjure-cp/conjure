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
rule_frameUpdate = "set-frameUpdate{Explicit}" `namedRule` theRule where
    theRule p = do
        (old, new, oldFocus, newFocus, cons) <- match opFrameUpdate p

        TypeSet{}    <- typeOf old
        Set_Explicit <- representationOf old
        [oldValues]  <- downX1 old
        (oldIndex:_) <- indexDomainsOf oldValues

        TypeSet{}    <- typeOf new
        Set_Explicit <- representationOf new
        [newValues]  <- downX1 new
        (newIndex:_) <- indexDomainsOf newValues

        return
            ( "Vertical rule for set-frameUpdate, Explicit representation"
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

                let nbOlds = Constant $ ConstantInt $ genericLength oldFocus
                let nbNews = Constant $ ConstantInt $ genericLength newFocus

                -- keep everything out of focus unchanged
                let freezeFrameCons =
                        let
                            k_is_b = make opOr  $ fromList [ [essence| &k = &i |]
                                                           | (_, _, i, _) <- focusNames_b
                                                           ]

                            maxOldIndex = [essence| max(`&oldIndex`) |]

                        in
                            if length newFocus >= length oldFocus
                                then
                                    [essence|
                                        forAll &kPat : int(1..&maxOldIndex) .
                                            (! &k_is_b) -> &newValues[&k] = &oldValues[&k + &offsets[&k]]
                                    |]
                                else
                                    [essence|
                                        forAll &kPat : int(1..&maxOldIndex - (&nbOlds - &nbNews)) .
                                            (! &k_is_b) -> &newValues[&k] = &oldValues[&k + &offsets[&k]]
                                    |]

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
                            [ Declaration (FindOrGiven LocalFind contiguousCountsPat
                                    (DomainMatrix oldIndex
                                        (DomainInt [RangeBounded
                                                        (Constant $ ConstantInt 0)
                                                        (Constant $ ConstantInt $ genericLength focusNames_a)])))
                            , Declaration (FindOrGiven LocalFind offsetsPat
                                    (DomainMatrix oldIndex
                                        (DomainInt [RangeBounded
                                                        (Constant $ ConstantInt $ negate $ maximum [genericLength focusNames_a, genericLength focusNames_b])
                                                        (Constant $ ConstantInt $          maximum [genericLength focusNames_a, genericLength focusNames_b])])))
                            ] ++
                            [ SuchThat
                                [ make opAllDiff (fromList [auxVar | (_,_,auxVar,_) <- focusNames_a])

                                , make opAllDiff (fromList [auxVar | (_,_,auxVar,_) <- focusNames_b])

                                , contiguousCountsCons
                                , buildOffSetsCons
                                , freezeFrameCons

                                , consOut
                                , impliedSize
                                ]
                            ])

                return out
            )


