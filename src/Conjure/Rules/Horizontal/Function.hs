{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Horizontal.Function where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.Lenses
import Conjure.Language.TH

import Conjure.Rules.Definition ( Rule(..), namedRule, hasRepresentation, matchFirst )

import Conjure.Representations ( downX1 )


rule_Comprehension_Literal :: Rule
rule_Comprehension_Literal = "function-comprehension-literal" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension_Literal"
        elems <- match functionLiteral expr
        let outLiteral = make matrixLiteral (DomainInt [RangeBounded 1 (fromInt $ length elems)])
                            [ AbstractLiteral (AbsLitTuple [a,b])
                            | (a,b) <- elems
                            ]
        let upd val old = lambdaToFunction pat old val
        return
            ( "Comprehension on function literals"
            , \ fresh ->
                 let (iPat, i) = quantifiedVar (fresh `at` 0)
                 in  Comprehension (upd i body)
                         $  gofBefore
                         ++ [Generator (GenInExpr iPat outLiteral)]
                         ++ transformBi (upd i) gofAfter
            )
    theRule _ = na "rule_Comprehension_Literal"


rule_Image_Literal :: Rule
rule_Image_Literal = "function-image-literal" `namedRule` theRule where
    theRule [essence| &lhs = &rhs |] = do
        (func, arg) <- match opFunctionImage lhs
        tyFunc      <- typeOf func
        isBool      <- case tyFunc of
            TypeFunction _ TypeBool -> -- out of range means "false = &rhs"
                return True
            _ -> -- out of range means "false"
                return False
        elems       <- match functionLiteral func
        let argIsUndef = make opNot $ make opOr $ fromList
                [ [essence| &a = &arg |]
                | (a,_) <- elems
                ]
        return $
            if null elems
                then
                    ( "Image of empty function literal"
                    , const [essence| false |]
                    )
                else
                    ( "Image of function literal"
                    , const $ if isBool
                        then
                            -- the argument is defined and equal to rhs
                            -- OR
                            -- the argument is undefined and rhs = false
                            make opOr $ fromList $
                                [ [essence| (&a = &arg) /\ (&b = &rhs) |]
                                | (a,b) <- elems
                                ] ++ [ [essence| &argIsUndef /\ (&rhs = false) |] ]
                        else
                            make opOr $ fromList
                                [ [essence| (&a = &arg) /\ (&b = &rhs) |]
                                | (a,b) <- elems
                                ]
                    )
    theRule _ = na "rule_Image_Literal"


rule_Eq :: Rule
rule_Eq = "function-eq" `namedRule` theRule where
    theRule p = do
        (x,y)          <- match opEq p
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return ( "Horizontal rule for function equality"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence|
                            (forAll &iPat in &x . &y(&i[1]) = &i[2])
                                /\
                            (forAll &iPat in &y . &x(&i[1]) = &i[2])
                                /\
                            defined(&x) = defined(&y)
                        |]
               )


rule_Neq :: Rule
rule_Neq = "function-neq" `namedRule` theRule where
    theRule [essence| &x != &y |] = do
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return ( "Horizontal rule for function dis-equality"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence|
                            (exists &iPat in &x . !(&i in &y))
                            \/
                            (exists &iPat in &y . !(&i in &x))
                        |]
               )
    theRule _ = na "rule_Neq"


rule_SubsetEq :: Rule
rule_SubsetEq = "function-subsetEq" `namedRule` theRule where
    theRule p = do
        (x,y)          <- match opSubsetEq p
        TypeFunction{} <- typeOf x
        TypeFunction{} <- typeOf y
        return ( "Horizontal rule for function subsetEq"
               , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                    in  [essence|
                            (forAll &iPat in &x . &y(&i[1]) = &i[2])
                                /\
                            defined(&x) subsetEq defined(&y)
                        |]
               )


rule_Subset :: Rule
rule_Subset = "function-subset" `namedRule` theRule where
    theRule [essence| &a subset &b |] = do
        TypeFunction{} <- typeOf a
        TypeFunction{} <- typeOf b
        return
            ( "Horizontal rule for set subset"
            , const [essence| &a subsetEq &b /\ &a != &b |]
            )
    theRule _ = na "rule_Subset"


rule_Supset :: Rule
rule_Supset = "set-supset" `namedRule` theRule where
    theRule [essence| &a supset &b |] = do
        TypeFunction{} <- typeOf a
        TypeFunction{} <- typeOf b
        return
            ( "Horizontal rule for set supset"
            , const [essence| &b subset &a |]
            )
    theRule _ = na "rule_Supset"


rule_SupsetEq :: Rule
rule_SupsetEq = "set-subsetEq" `namedRule` theRule where
    theRule [essence| &a supsetEq &b |] = do
        TypeFunction{} <- typeOf a
        TypeFunction{} <- typeOf b
        return
            ( "Horizontal rule for set supsetEq"
            , const [essence| &b subsetEq &a |]
            )
    theRule _ = na "rule_SupsetEq"


rule_Lt :: Rule
rule_Lt = "function-lt" `namedRule` theRule where
    theRule p = do
        (a,b) <- match opLt p
        TypeFunction{} <- typeOf a
        TypeFunction{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- tupleLitIfNeeded <$> downX1 a
        mb <- tupleLitIfNeeded <$> downX1 b
        return ( "Horizontal rule for function <" <+> pretty (make opLt ma mb)
               , const $ make opLt ma mb
               )


rule_Leq :: Rule
rule_Leq = "function-leq" `namedRule` theRule where
    theRule p = do
        (a,b) <- match opLeq p
        TypeFunction{} <- typeOf a
        TypeFunction{} <- typeOf b
        hasRepresentation a
        hasRepresentation b
        ma <- tupleLitIfNeeded <$> downX1 a
        mb <- tupleLitIfNeeded <$> downX1 b
        return ( "Horizontal rule for function <=" <+> pretty (make opLeq ma mb)
               , const $ make opLeq ma mb
               )


rule_Comprehension_PreImage :: Rule
rule_Comprehension_PreImage = "function-preImage" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_PreImage"
        (func, img) <- match opPreImage expr
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over the preImage of a function"
            , \ fresh ->
                let
                    (jPat, j) = quantifiedVar (fresh `at` 0)
                    val = [essence| &j[1] |]
                in
                    Comprehension
                        (upd val body)
                        $  gofBefore
                        ++ [ Generator (GenInExpr jPat func)
                           , Condition [essence| &j[2] = &img |]
                           ]
                        ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension_PreImage"


rule_Card :: Rule
rule_Card = "function-cardinality" `namedRule` theRule where
    theRule [essence| |&f| |] = do
        TypeFunction{} <- typeOf f
        return
            ( "Function cardinality"
            , const [essence| |toSet(&f)| |]
            )
    theRule _ = na "rule_Card"


-- | TODO: This may allow repetitions.
rule_Comprehension_Defined :: Rule
rule_Comprehension_Defined = "function-defined" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_PreImage"
        func <- match opDefined expr
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over defined(f)"
            , \ fresh ->
                    let
                        (jPat, j) = quantifiedVar (fresh `at` 0)
                        val = [essence| &j[1] |]
                    in
                        Comprehension
                            (upd val body)
                            $  gofBefore
                            ++ [ Generator (GenInExpr jPat func) ]
                            ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension_Defined"


-- | TODO: This may allow repetitions.
rule_Comprehension_Range :: Rule
rule_Comprehension_Range = "function-range" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_PreImage"
        func <- match opRange expr
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over range(f)"
            , \ fresh ->
                    let
                        (jPat, j) = quantifiedVar (fresh `at` 0)
                        val = [essence| &j[2] |]
                    in
                        Comprehension
                            (upd val body)
                            $  gofBefore
                            ++ [ Generator (GenInExpr jPat func) ]
                            ++ transformBi (upd val) gofAfter
            )
    theRule _ = na "rule_Comprehension_Range"


rule_In :: Rule
rule_In = "function-in" `namedRule` theRule where
    theRule [essence| &x in &f |] = do
        TypeFunction{} <- typeOf f
        return
            ( "Function membership to function image."
            , const [essence| &f(&x[1]) = &x[2] |]
            )
    theRule _ = na "rule_In"


rule_Restrict_Image :: Rule
rule_Restrict_Image = "function-restrict-image" `namedRule` theRule where
    theRule p = do
        (func', arg) <- match opFunctionImage p
        (func , dom) <- match opRestrict func'
        return
            ( "Function image on a restricted function."
            , \ fresh ->
                    let (iPat, i) = quantifiedVar (fresh `at` 0)
                        bob = [essence| exists &iPat : &dom . &i = &arg |]
                    in  WithLocals (make opFunctionImage func arg) [SuchThat [bob]]
            )


rule_Restrict_Comprehension :: Rule
rule_Restrict_Comprehension = "function-restrict-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (iPat, iPatName, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr iPat@(Single iPatName) expr) -> return (iPat, iPatName, expr)
            _ -> na "rule_Comprehension_PreImage"
        (func, dom) <- match opRestrict expr
        return
            ( "Mapping over restrict(func, dom)"
            , \ fresh ->
                    let (jPat, j) = quantifiedVar (fresh `at` 0)
                        i = Reference iPatName Nothing
                    in
                        Comprehension body
                            $  gofBefore
                            ++ [ Generator (GenInExpr iPat func)
                               , Condition [essence| exists &jPat : &dom . &j = &i[1] |]
                               ]
                            ++ gofAfter
            )
    theRule _ = na "rule_Restrict_Comprehension"


rule_Mk_FunctionImage :: Rule
rule_Mk_FunctionImage = "mk-function-image" `namedRule` theRule where
    theRule p = do
        (f, [Just arg]) <- match opRelationProj p
        TypeFunction{}  <- typeOf f
        return
            ( "This is a function image."
            , const $ make opFunctionImage f arg
            )


-- | image(f,x) can be nasty for non-total functions.
--   1.   if f is a total function, it can readily be replaced by a set expression.
--   2.1. if f isn't total, and if the return type is right, it will always end up as a generator for a comprehension.
--      a vertical rule is needed for such cases.
--   2.2. if the return type is not "right", i.e. it is a bool or an int, i.e. sth we cannot quantify over,
--        the vertical rule is harder.



rule_Comprehension_Image :: Rule
rule_Comprehension_Image = "function-image" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension_Image"
        (func, arg) <- match opFunctionImage expr
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over the image of a function"
            , \ fresh ->
                let
                    (iPat, i) = quantifiedVar (fresh `at` 0)
                    (jPat, j) = quantifiedVar (fresh `at` 1)
                in
                    Comprehension
                        (upd j body)
                        $  gofBefore
                        ++ [ Generator (GenInExpr iPat func)
                           , Condition [essence| &i[1] = &arg |]
                           , Generator (GenInExpr jPat [essence| &i[2] |])
                           ]
                        ++ transformBi (upd j) gofAfter
            )
    theRule _ = na "rule_Comprehension_Image"

