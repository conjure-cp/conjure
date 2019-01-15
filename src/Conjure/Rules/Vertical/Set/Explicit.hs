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
                DomainInt _ [RangeBounded lb _] -> return lb
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
                DomainInt _ [RangeBounded _ ub] -> return ub
                _ -> do
                    (jPat, j) <- quantifiedVar
                    return [essence| max([&j | &jPat : &index]) |]
        return
            ( "Vertical rule for set max, Explicit representation."
            , return [essence| &m[&maxInIndex] |]
            )
