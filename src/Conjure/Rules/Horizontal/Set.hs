{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Set where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.DomainOf
import Conjure.Language.CategoryOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule, hasRepresentation, matchFirst )

import Conjure.Representations ( downX1 )


rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "set-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Literal"
        elems <- match setLiteral expr
        let outLiteral = make matrixLiteral (DomainInt [RangeBounded 1 (fromInt $ length elems)]) elems
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on set literals"
            , \ fresh ->
                 let (iPat, i) = quantifiedVar (fresh `at` 0)
                 in  Comprehension (upd i body)
                         $  gofBefore
                         ++ [Generator (GenInExpr iPat outLiteral)]
                         ++ transformBi (upd i) gofAfter
            )
    theRule _ = na "rule_Comprehension_Literal"


rule_Eq :: Rule
rule_Eq = "set-eq" `namedRule` theRule where
    theRule p = do
        (x,y)     <- match opEq p
        TypeSet{} <- typeOf x
        TypeSet{} <- typeOf y
        return ( "Horizontal rule for set equality"
               , const $ make opAnd $ fromList
                   [ make opSubsetEq x y
                   , make opSubsetEq y x
                   ]
               )


rule_Neq :: Rule
rule_Neq = "set-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypeSet{} <- typeOf x
        TypeSet{} <- typeOf y
        return ( "Horizontal rule for set dis-equality"
               , const [essence| !(&x = &y) |]
               )
    theRule _ = na "rule_Neq"


rule_SubsetEq :: Rule
rule_SubsetEq = "set-subsetEq" `namedRule` theRule where
    theRule p = do
        (x,y)     <- match opSubsetEq p
        TypeSet{} <- typeOf x
        TypeSet{} <- typeOf y
        return ( "Horizontal rule for set subsetEq"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| forAll &iPat in (&x) . &i in &y |]
               )


rule_Subset :: Rule
rule_Subset = "set-subset" `namedRule` theRule where
    theRule [essence| &a subset &b |] = do
        TypeSet{} <- typeOf a
        TypeSet{} <- typeOf b
        return
            ( "Horizontal rule for set subset"
            , const [essence| &a subsetEq &b /\ &a != &b |]
            )
    theRule _ = na "rule_Subset"


rule_Supset :: Rule
rule_Supset = "set-supset" `namedRule` theRule where
    theRule [essence| &a supset &b |] = do
        TypeSet{} <- typeOf a
        TypeSet{} <- typeOf b
        return
            ( "Horizontal rule for set supset"
            , const [essence| &b subset &a |]
            )
    theRule _ = na "rule_Supset"


rule_SupsetEq :: Rule
rule_SupsetEq = "set-subsetEq" `namedRule` theRule where
    theRule [essence| &a supsetEq &b |] = do
        TypeSet{} <- typeOf a
        TypeSet{} <- typeOf b
        return
            ( "Horizontal rule for set supsetEq"
            , const [essence| &b subsetEq &a |]
            )
    theRule _ = na "rule_SupsetEq"


rule_Lt :: Rule
rule_Lt = "set-lt" `namedRule` theRule where
    theRule p = do
        (a,b)     <- match opLt p
        TypeSet{} <- typeOf a
        TypeSet{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- tupleLitIfNeeded <$> downX1 a
        mb <- tupleLitIfNeeded <$> downX1 b
        return ( "Horizontal rule for set <" <+> pretty (make opLt ma mb)
               , const $ make opLt ma mb
               )


rule_Leq :: Rule
rule_Leq = "set-leq" `namedRule` theRule where
    theRule p = do
        (a,b)     <- match opLeq p
        TypeSet{} <- typeOf a
        TypeSet{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- tupleLitIfNeeded <$> downX1 a
        mb <- tupleLitIfNeeded <$> downX1 b
        return ( "Horizontal rule for set <=" <+> pretty (make opLeq ma mb)
               , const $ make opLeq ma mb
               )


rule_Intersect :: Rule
rule_Intersect = "set-intersect" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, iPat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@(Single iPat) expr) ->
                return (pat, iPat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Intersect"
        (x, y)             <- match opIntersect expr
        tx                 <- typeOf x
        case tx of
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            TypeRelation{} -> return ()
            _              -> fail "type incompatibility in intersect operator"
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for set intersection"
            , const $
                Comprehension body
                    $  gofBefore
                    ++ [ Generator (GenInExpr pat x)
                       , Condition [essence| &i in &y |]
                       ]
                    ++ gofAfter
            )
    theRule _ = na "rule_Intersect"


rule_Union :: Rule
rule_Union = "set-union" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, iPat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@(Single iPat) expr) -> return (pat, iPat, matchDef opToSet expr)
            _ -> na "rule_Union"
        (x, y)             <- match opUnion expr
        tx                 <- typeOf x
        case tx of
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            TypeRelation{} -> return ()
            _              -> fail "type incompatibility in union operator"
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for set union"
            , const $ make opFlatten $ AbstractLiteral $ AbsLitMatrix
                (DomainInt [RangeBounded 1 2])
                [ Comprehension body
                    $  gofBefore
                    ++ [ Generator (GenInExpr pat x) ]
                    ++ gofAfter
                , Comprehension body
                    $  gofBefore
                    ++ [ Generator (GenInExpr pat y)
                       , Condition [essence| !(&i in &x) |]
                       ]
                    ++ gofAfter
                ]
            )
    theRule _ = na "rule_Union"


rule_Difference :: Rule
rule_Difference = "set-difference" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, iPat, s), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@(Single iPat) s) -> return (pat, iPat, s)
            _ -> na "rule_Difference"
        (x, y)             <- match opMinus s
        tx                 <- typeOf x
        case tx of
            TypeSet{}      -> return ()
            TypeMSet{}     -> return ()
            TypeFunction{} -> return ()
            TypeRelation{} -> return ()
            _              -> fail "type incompatibility in difference operator"
        let i = Reference iPat Nothing
        return
            ( "Horizontal rule for set difference"
            , const $
                Comprehension body
                    $  gofBefore
                    ++ [ Generator (GenInExpr pat x)
                       , Condition [essence| !(&i in &y) |]
                       ]
                    ++ gofAfter
            )
    theRule _ = na "rule_Difference"


rule_PowerSet_Difference :: Rule
rule_PowerSet_Difference = "set-powerSet-difference" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat expr) -> return (pat, expr)
            _ -> na "rule_PowerSet_Difference"
        setExpr            <- match opPowerSet expr
        (x, y)             <- match opMinus setExpr
        let patAsExpr = patternToExpr pat
        return
            ( "Horizontal rule for set powerSet difference"
            , const $
                Comprehension body
                    $  gofBefore
                    ++ [ Generator (GenInExpr pat (make opPowerSet x))
                       , Condition [essence| !(&patAsExpr subsetEq &y) |]
                       ]
                    ++ gofAfter
            )
    theRule _ = na "rule_PowerSet_Difference"


rule_MaxMin :: Rule
rule_MaxMin = "set-max-min" `namedRule` theRule where
    theRule [essence| max(&s) |] = do
        TypeSet TypeInt <- typeOf s
        return
            ( "Horizontal rule for set max"
            , \ fresh ->
                let (iPat, i) = quantifiedVar (fresh `at` 0)
                in  [essence| max([&i | &iPat <- &s]) |]
            )
    theRule [essence| min(&s) |] = do
        TypeSet TypeInt <- typeOf s
        return
            ( "Horizontal rule for set min"
            , \ fresh ->
                let (iPat, i) = quantifiedVar (fresh `at` 0)
                in  [essence| min([&i | &iPat <- &s]) |]
            )
    theRule _ = na "rule_MaxMin"


-- x in s ~~> or([ x = i | i in s ])
rule_In :: Rule
rule_In = "set-in" `namedRule` theRule where
    theRule p = do
        (x,s)     <- match opIn p
        TypeSet{} <- typeOf s
        return ( "Horizontal rule for set-in."
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| exists &iPat in &s . &i = &x |]
               )


rule_Card :: Rule
rule_Card = "set-card" `namedRule` theRule where
    theRule p = do
        s         <- match opTwoBars p
        TypeSet{} <- typeOf s
        return ( "Horizontal rule for set cardinality."
               , \ fresh ->
                    let (iPat, _) = quantifiedVar (fresh `at` 0)
                    in  [essence| sum &iPat in &s . 1 |]
               )


rule_Param_MinOfSet :: Rule
rule_Param_MinOfSet = "param-min-of-set" `namedRule` theRule where
    theRule [essence| min(&s) |] = do
        TypeSet TypeInt <- typeOf s
        unless (categoryOf s == CatParameter) $ na "rule_Param_MinOfSet"
        hasRepresentation s
        DomainSet _ _ inner <- domainOf s
        return
            ( "min of a parameter set"
            , case inner of
                DomainInt [RangeBounded l _] -> const l
                _ -> \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| min([ &i | &iPat : &inner ]) |]
            )
    theRule _ = na "rule_Param_MinOfSet"


rule_Param_MaxOfSet :: Rule
rule_Param_MaxOfSet = "param-max-of-set" `namedRule` theRule where
    theRule [essence| max(&s) |] = do
        TypeSet TypeInt <- typeOf s
        unless (categoryOf s == CatParameter) $ na "rule_Param_MaxOfSet"
        hasRepresentation s
        DomainSet _ _ inner <- domainOf s
        return
            ( "max of a parameter set"
            , case inner of
                DomainInt [RangeBounded _ u] -> const u
                _ -> \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence| max([ &i | &iPat : &inner ]) |]
            )
    theRule _ = na "rule_Param_MaxOfSet"
