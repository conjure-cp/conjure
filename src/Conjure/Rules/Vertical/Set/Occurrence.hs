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
        (old, new, oldFocus, newFocus, cons) <- match opFrameUpdate p

        TypeSet{}      <- typeOf old
        Set_Occurrence <- representationOf old
        [oldValues]    <- downX1 old
        (oldIndex:_)   <- indexDomainsOf oldValues

        TypeSet{}      <- typeOf new
        Set_Occurrence <- representationOf new
        [newValues]    <- downX1 new
        (newIndex:_)   <- indexDomainsOf newValues

        return
            ( "Vertical rule for set-frameUpdate, Occurrence representation"
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
                                ([auxVar], _) -> auxVar
                                (_, [auxVar]) -> auxVar
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
                                , make opAnd     (fromList [ [essence| &oldValues[&auxVar] |]
                                                           | (_,_,auxVar,_) <- focusNames_a ])

                                , make opAllDiff (fromList [auxVar | (_,_,auxVar,_) <- focusNames_b])
                                , make opAnd     (fromList [ [essence| &newValues[&auxVar] |]
                                                           | (_,_,auxVar,_) <- focusNames_b ])

                                , contiguousCountsCons
                                , buildOffSetsCons
                                , freezeFrameCons

                                , consOut
                                , impliedSize
                                ]
                            ])

                return out
            )

