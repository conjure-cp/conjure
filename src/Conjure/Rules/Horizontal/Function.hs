{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.Rules.Horizontal.Function where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.DomainOf
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
        (TypeFunction fr to, elems) <- match functionLiteral expr
        let outLiteral = make matrixLiteral
                            (TypeMatrix TypeInt (TypeTuple [fr,to]))
                            (DomainInt [RangeBounded 1 (fromInt (genericLength elems))])
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


rule_Image_Literal_Bool :: Rule
rule_Image_Literal_Bool = "function-image-literal-bool" `namedRule` theRule where
    theRule p = do
        (func, arg)                      <- match opFunctionImage p
        (TypeFunction _ TypeBool, elems) <- match functionLiteral func
        -- let argIsUndef = make opNot $ make opOr $ fromList
        --         [ [essence| &a = &arg |]
        --         | (a,_) <- elems
        --         ]
        return $
            if null elems
                then
                    ( "Image of empty function literal"
                    , const [essence| false |]                          -- undefined is false.
                    )
                else
                    ( "Image of function literal"
                    , const $ make opOr $ fromList $
                          [ [essence| (&a = &arg) /\ &b |]              -- if this is ever true, the output is true.
                                                                        -- undefined is still false.
                          | (a,b) <- elems
                          ]
                    )


rule_Image_Literal_Int :: Rule
rule_Image_Literal_Int = "function-image-literal-int" `namedRule` theRule where
    theRule p = do
        (func, arg)                     <- match opFunctionImage p
        (TypeFunction _ TypeInt, elems) <- match functionLiteral func
        return
            ( "Image of function literal"
            , const $
                let
                    val = make opSum $ fromList $
                        -- if this is ever true, the output is the value of b.
                        [ [essence| toInt(&a = &arg) * &b |]
                        | (a,b) <- elems
                        ]
                    argIsDef = make opOr $ fromList
                        [ [essence| &a = &arg |]
                        | (a,_) <- elems
                        ]
                in
                    WithLocals val [SuchThat [argIsDef]]
            )


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


rule_Comprehension_Defined :: Rule
rule_Comprehension_Defined = "function-defined" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Defined"
        func <- match opDefined expr
        DomainFunction _ _ domFr _domTo <- domainOf func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over defined(f)"
            , \ fresh ->
                    let
                        (auxName, aux) = auxiliaryVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)
                        (kPat, k) = quantifiedVar (fresh `at` 2)
                        (lPat, l) = quantifiedVar (fresh `at` 3)
                        k1 = [essence| &k[1] |]
                        l1 = [essence| &l[1] |]
                    in
                        WithLocals
                            (Comprehension
                                (upd j body)
                                $  gofBefore
                                ++ [ Generator (GenInExpr jPat aux) ]
                                ++ transformBi (upd j) gofAfter)
                            [ Declaration (FindOrGiven LocalFind auxName (DomainSet def def (forgetRepr domFr)))
                            , SuchThat
                                [ make opAnd $ Comprehension
                                    [essence| &k1 in &aux |]
                                    [ Generator (GenInExpr kPat func) ]
                                , make opAnd $
                                    Comprehension
                                        (make opOr $ Comprehension
                                            [essence| &l1 = &k |]
                                            [ Generator (GenInExpr lPat func) ]
                                        )
                                        [ Generator (GenInExpr kPat aux) ]
                                ]
                            ]
            )
    theRule _ = na "rule_Comprehension_Defined"


rule_Comprehension_Range :: Rule
rule_Comprehension_Range = "function-range" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Range"
        func <- match opRange expr
        DomainFunction _ _ _domFr domTo <- domainOf func
        let upd val old = lambdaToFunction pat old val
        return
            ( "Mapping over range(f)"
            , \ fresh ->
                    let
                        (auxName, aux) = auxiliaryVar (fresh `at` 0)
                        (jPat, j) = quantifiedVar (fresh `at` 1)
                        (kPat, k) = quantifiedVar (fresh `at` 2)
                        (lPat, l) = quantifiedVar (fresh `at` 3)
                        k1 = [essence| &k[1] |]
                        l1 = [essence| &l[1] |]
                    in
                        WithLocals
                            (Comprehension
                                (upd j body)
                                $  gofBefore
                                ++ [ Generator (GenInExpr jPat aux) ]
                                ++ transformBi (upd j) gofAfter)
                            [ Declaration (FindOrGiven LocalFind auxName (DomainSet def def (forgetRepr domTo)))
                            , SuchThat
                                [ make opAnd $ Comprehension
                                    [essence| &k1 in &aux |]
                                    [ Generator (GenInExpr kPat func) ]
                                , make opAnd $
                                    Comprehension
                                        (make opOr $ Comprehension
                                            [essence| &l1 = &k |]
                                            [ Generator (GenInExpr lPat func) ]
                                        )
                                        [ Generator (GenInExpr kPat aux) ]
                                ]
                            ]
            )
    theRule _ = na "rule_Comprehension_Range"


rule_Comprehension_Defined_Size :: Rule
rule_Comprehension_Defined_Size = "function-defined-size" `namedRule` theRule where
    theRule [essence| size(defined(&func), &n) |] = do
        DomainFunction _ _ domFr _domTo <- domainOf func
        return
            ( "size(defined(func), n)"
            , \ fresh ->
                    let
                        (auxName, aux) = auxiliaryVar (fresh `at` 0)
                        (kPat, k) = quantifiedVar (fresh `at` 2)
                        (lPat, l) = quantifiedVar (fresh `at` 3)
                        k2 = [essence| &k[2] |]
                        l2 = [essence| &l[2] |]
                    in
                        WithLocals
                            (fromBool True)
                            [ Declaration (FindOrGiven LocalFind auxName
                                    (DomainSet def (SetAttr (SizeAttr_Size n)) (forgetRepr domFr)))
                            , SuchThat
                                [ make opAnd $ Comprehension
                                    [essence| &k2 in &aux |]
                                    [ Generator (GenInExpr kPat func) ]
                                , make opAnd $
                                    Comprehension
                                        (make opOr $ Comprehension
                                            [essence| &l2 = &k |]
                                            [ Generator (GenInExpr lPat func) ]
                                        )
                                        [ Generator (GenInExpr kPat aux) ]
                                ]
                            ]
            )
    theRule _ = na "rule_Comprehension_Defined_Size"


rule_Comprehension_Range_Size :: Rule
rule_Comprehension_Range_Size = "function-range-size" `namedRule` theRule where
    theRule [essence| size(range(&func), &n) |] = do
        DomainFunction _ _ _domFr domTo <- domainOf func
        return
            ( "size(range(func), n)"
            , \ fresh ->
                    let
                        (auxName, aux) = auxiliaryVar (fresh `at` 0)
                        (kPat, k) = quantifiedVar (fresh `at` 2)
                        (lPat, l) = quantifiedVar (fresh `at` 3)
                        k2 = [essence| &k[2] |]
                        l2 = [essence| &l[2] |]
                    in
                        WithLocals
                            (fromBool True)
                            [ Declaration (FindOrGiven LocalFind auxName
                                    (DomainSet def (SetAttr (SizeAttr_Size n)) (forgetRepr domTo)))
                            , SuchThat
                                [ make opAnd $ Comprehension
                                    [essence| &k2 in &aux |]
                                    [ Generator (GenInExpr kPat func) ]
                                , make opAnd $
                                    Comprehension
                                        (make opOr $ Comprehension
                                            [essence| &l2 = &k |]
                                            [ Generator (GenInExpr lPat func) ]
                                        )
                                        [ Generator (GenInExpr kPat aux) ]
                                ]
                            ]
            )
    theRule _ = na "rule_Comprehension_Range_Size"


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

rule_Image_Bool :: Rule
rule_Image_Bool = "function-image-bool" `namedRule` theRule where
    theRule p = do
        let
            onChildren
                :: MonadState (Maybe (Expression, Expression)) m
                => Expression
                -> m (Expression -> Expression)
            onChildren ch = do
                let
                    try = do
                        (func, arg) <- match opFunctionImage ch
                        case match opRestrict func of
                            Nothing -> return ()
                            Just{}  -> na "rule_Image_Bool"         -- do not use this rule for restricted functions
                        TypeFunction _ TypeBool <- typeOf func
                        return (func, arg)
                case try of
                    Nothing -> return (const ch)        -- do not fail if a child is not of proper form
                    Just (func, arg) -> do              -- just return it back unchanged
                        seenBefore <- gets id
                        case seenBefore of
                            Nothing -> do
                                modify $ const $ Just (func, arg)
                                return id
                            Just{}  ->
                                return (const ch)

        let (children_, gen) = uniplate p
        (genChildren, mFunc) <- runStateT (mapM onChildren children_) Nothing
        let
            mkP :: Expression -> Expression
            mkP new = gen $ fmap ($ new) genChildren
        (func, arg) <- maybe (na "rule_Image_Bool") return mFunc        -- Nothing signifies no relevant children
        return
            ( "Function image, bool."
            , \ fresh ->
                let
                    (iPat, i) = quantifiedVar (fresh `at` 0)
                in
                    mkP $ make opOr $ Comprehension [essence| &i[2] |]
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
            )


rule_Image_Int :: Rule
rule_Image_Int = "function-image-int" `namedRule` theRule where
    theRule p = do
        let
            onChildren
                :: MonadState (Maybe (Expression, Expression)) m
                => Expression
                -> m (Expression -> Expression)
            onChildren ch = do
                let
                    try = do
                        (func, arg) <- match opFunctionImage ch
                        case match opRestrict func of
                            Nothing -> return ()
                            Just{}  -> na "rule_Image_Int"          -- do not use this rule for restricted functions
                        TypeFunction _ TypeInt <- typeOf func
                        return (func, arg)
                case try of
                    Nothing -> return (const ch)        -- do not fail if a child is not of proper form
                    Just (func, arg) -> do              -- just return it back unchanged
                        seenBefore <- gets id
                        case seenBefore of
                            Nothing -> do
                                modify $ const $ Just (func, arg)
                                return id
                            Just{}  ->
                                return (const ch)

        let (children_, gen) = uniplate p
        (genChildren, mFunc) <- runStateT (mapM onChildren children_) Nothing
        let
            mkP :: Expression -> Expression
            mkP new = gen $ fmap ($ new) genChildren
        (func, arg) <- maybe (na "rule_Image_Int") return mFunc         -- Nothing signifies no relevant children
        return
            ( "Function image, int."
            , \ fresh ->
                let
                    (iPat, i) = quantifiedVar (fresh `at` 0)
                    val = make opSum $ Comprehension [essence| &i[2] |]
                        [ Generator (GenInExpr iPat func)
                        , Condition [essence| &i[1] = &arg |]
                        ]
                    isDefined = [essence| &arg in defined(&func) |]
                in
                    mkP $ WithLocals val [SuchThat [isDefined]]
            )


rule_Comprehension_Image :: Rule
rule_Comprehension_Image = "function-image-comprehension" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, expr), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, expr)
            _ -> na "rule_Comprehension_Image"
        (mkModifier, expr2) <- match opModifier expr
        (func, arg) <- match opFunctionImage expr2
        case match opRestrict func of
            Nothing -> return ()
            Just{}  -> na "rule_Image_Bool"         -- do not use this rule for restricted functions
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
                        ++ [ Generator (GenInExpr iPat (mkModifier func))
                           , Condition [essence| &i[1] = &arg |]
                           , Generator (GenInExpr jPat [essence| &i[2] |])
                           ]
                        ++ transformBi (upd j) gofAfter
            )
    theRule _ = na "rule_Comprehension_Image"
