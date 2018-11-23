{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Relation where

import Conjure.Rules.Import


rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "relation-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension_Literal"
        (TypeRelation taus, elems) <- match relationLiteral expr
        let outLiteral = make matrixLiteral
                            (TypeMatrix (TypeInt NoTag) (TypeTuple taus))
                            (DomainInt NoTag [RangeBounded 1 (fromInt (genericLength elems))])
                            [ AbstractLiteral (AbsLitTuple row)
                            | row <- elems
                            ]
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on relation literals"
            , do
                 (iPat, i) <- quantifiedVar
                 return $ Comprehension (upd i body)
                         $  gocBefore
                         ++ [Generator (GenInExpr iPat outLiteral)]
                         ++ transformBi (upd i) gocAfter
            )
    theRule _ = na "rule_Comprehension_Literal"


-- [ body | i <- rel(...) ]
-- [ body | jPat <- rel(...), j =   ]
rule_Comprehension_Projection :: Rule
rule_Comprehension_Projection = "relation-comprehension-projection" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDef opToSet expr)
            _ -> na "rule_Comprehension_Projection"
        (rel, args)    <- match opRelationProj expr
        TypeRelation{} <- typeOf rel
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on relation literals"
            , do
                (jPat, j) <- quantifiedVar
                    -- those indices to keep
                let val = AbstractLiteral $ AbsLitTuple
                        [ [essence| &j[&iExpr] |]
                        | (i, Nothing) <- zip allNats args
                        , let iExpr = fromInt i
                        ]
                let conditions =
                        [ Condition [essence| &j[&iExpr] = &arg |]
                        | (i, Just arg) <- zip allNats args
                        , let iExpr = fromInt i
                        ]
                return $ Comprehension
                   (upd val body)
                   $  gocBefore
                   ++ [Generator (GenInExpr jPat rel)]
                   ++ conditions
                   ++ transformBi (upd val) gocAfter
            )
    theRule _ = na "rule_Comprehension_Projection"


rule_PowerSet_Comprehension :: Rule
rule_PowerSet_Comprehension = "relation-powerSet-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (setPat, setPatNum, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr setPat@(AbsPatSet pats) expr) -> return (setPat, length pats, expr)
            _ -> na "rule_PowerSet_Comprehension"
        let upd val old      = lambdaToFunction setPat old val
        rel                  <- match opPowerSet expr
        TypeRelation{}       <- typeOf rel
        return
            ( "Horizontal rule for powerSet relation-comprehension"
            , do
                outPats <- replicateM setPatNum quantifiedVar
                let val = AbstractLiteral $ AbsLitSet [ j | (_,j) <- outPats ]
                return $
                    Comprehension (upd val body) $ concat
                        [ gocBefore
                        , concat
                            [ [ Generator (GenInExpr pat rel) ]
                            | (pat,_) <- take 1 outPats
                            ]
                        , concat
                            [ [ Generator (GenInExpr pat rel)
                              , Condition [essence| &beforeX < &patX |]
                              ]
                            | ((_, beforeX), (pat, patX)) <- zip outPats (tail outPats)
                            ]
                        , transformBi (upd val) gocAfter
                        ]
            )
    theRule _ = na "rule_PowerSet_Comprehension"


rule_Eq :: Rule
rule_Eq = "relation-eq" `namedRule` theRule where
    theRule p = do
        (x,y)          <- match opEq p
        TypeRelation{} <- typeOf x
        TypeRelation{} <- typeOf y
        return
            ( "Horizontal rule for relation equality"
            , do
                 (iPat, i) <- quantifiedVar
                 return
                     [essence|
                         (forAll &iPat in &x . &i in &y)
                         /\
                         (forAll &iPat in &y . &i in &x)
                         /\
                         (|&x| = |&y|)
                     |]
            )


rule_Neq :: Rule
rule_Neq = "relation-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypeRelation{} <- typeOf x
        TypeRelation{} <- typeOf y
        return
            ( "Horizontal rule for relation dis-equality"
            , do
                 (iPat, i) <- quantifiedVar
                 return
                     [essence|
                         (exists &iPat in &x . !(&i in &y))
                         \/
                         (exists &iPat in &y . !(&i in &x))
                     |]
            )
    theRule _ = na "rule_Neq"


rule_SubsetEq :: Rule
rule_SubsetEq = "relation-subsetEq" `namedRule` theRule where
    theRule [essence| &x subsetEq &y |] = do
        TypeRelation{} <- typeOf x
        TypeRelation{} <- typeOf y
        return
            ( "Horizontal rule for relation subsetEq"
            , do
                 (iPat, i) <- quantifiedVar
                 return [essence| forAll &iPat in (&x) . &i in &y |]
            )
    theRule _ = na "rule_SubsetEq"


rule_Subset :: Rule
rule_Subset = "relation-subset" `namedRule` theRule where
    theRule [essence| &a subset &b |] = do
        TypeRelation{} <- typeOf a
        TypeRelation{} <- typeOf b
        return
            ( "Horizontal rule for relation subset"
            , return [essence| &a subsetEq &b /\ &a != &b |]
            )
    theRule _ = na "rule_Subset"


rule_Supset :: Rule
rule_Supset = "relation-supset" `namedRule` theRule where
    theRule [essence| &a supset &b |] = do
        TypeRelation{} <- typeOf a
        TypeRelation{} <- typeOf b
        return
            ( "Horizontal rule for relation supset"
            , return [essence| &b subset &a |]
            )
    theRule _ = na "rule_Supset"


rule_SupsetEq :: Rule
rule_SupsetEq = "relation-subsetEq" `namedRule` theRule where
    theRule [essence| &a supsetEq &b |] = do
        TypeRelation{} <- typeOf a
        TypeRelation{} <- typeOf b
        return
            ( "Horizontal rule for relation supsetEq"
            , return [essence| &b subsetEq &a |]
            )
    theRule _ = na "rule_SupsetEq"


rule_Image :: Rule
rule_Image = "relation-image" `namedRule` theRule where
    theRule p = do
        (rel, args)    <- match opRelationImage p
        TypeRelation{} <- typeOf rel
        let arg = AbstractLiteral (AbsLitTuple args)
        return
            ( "relation image to relation membership"
            , return [essence| &arg in &rel |]
            )


rule_In :: Rule
rule_In = "relation-in" `namedRule` theRule where
    theRule [essence| &x in &rel |] = do
        TypeRelation{} <- typeOf rel
        return
            ( "relation membership to existential quantification"
            , do
                (iPat, i) <- quantifiedVar
                return [essence| exists &iPat in toSet(&rel) . &i = &x |]
            )
    theRule _ = na "rule_In"


rule_Card :: Rule
rule_Card = "relation-cardinality" `namedRule` theRule where
    theRule [essence| |&x| |] = do
        TypeRelation{} <- typeOf x
        return
            ( "Relation cardinality"
            , return [essence| |toSet(&x)| |]
            )
    theRule _ = na "rule_Card"


rule_Param_Card :: Rule
rule_Param_Card = "param-card-of-relation" `namedRule` theRule where
    theRule [essence| |&x| |] = do
        TypeRelation _ <- typeOf x
        unless (categoryOf x == CatParameter) $ na "rule_Param_Card"
        DomainRelation _ (RelationAttr (SizeAttr_Size n) _) _ <- domainOf x
        return
            ( "cardinality of a parameter relation"
            , return n
            )
    theRule _ = na "rule_Param_Card"
