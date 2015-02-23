{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.FunctionAsRelation where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.TH
import Conjure.Language.Lenses

import Conjure.Rules.Definition ( Rule(..), namedRule, representationOf, matchFirst )

import Conjure.Representations ( downX1 )


-- TODO: this is incomplete
rule_Image_Eq :: Rule
rule_Image_Eq = "function-image-eq{FunctionAsRelation}" `namedRule` theRule where

    -- =
    theRule [essence| image(&func, &x) = &y |] = do
        "FunctionAsRelation" <- representationOf func
        [rel]                <- downX1 func
        return
            ( "Function image-equals, FunctionAsRelation representation"
            , \ fresh ->
                let
                    (iPat, i) = quantifiedVar (fresh `at` 0)
                in
                    [essence| or([ &i[1] = &x /\ &i[2] = &y | &iPat <- &rel ]) |]
            )
    theRule [essence| &y = image(&func, &x) |] = do
        "FunctionAsRelation" <- representationOf func
        [rel]                <- downX1 func
        return
            ( "Function image-equals, FunctionAsRelation representation"
            , \ fresh ->
                let
                    (iPat, i) = quantifiedVar (fresh `at` 0)
                in
                    [essence| or([ &i[1] = &x /\ &i[2] = &y | &iPat <- &rel ]) |]
            )

    -- <=
    theRule [essence| image(&func, &x) <= &y |] = do
        "FunctionAsRelation" <- representationOf func
        [rel]                <- downX1 func
        return
            ( "Function image-equals, FunctionAsRelation representation"
            , \ fresh ->
                let
                    (iPat, i) = quantifiedVar (fresh `at` 0)
                in
                    [essence| or([ &i[1] = &x /\ &i[2] <= &y | &iPat <- &rel ]) |]
            )
    theRule [essence| &y >= image(&func, &x) |] = do
        "FunctionAsRelation" <- representationOf func
        [rel]                <- downX1 func
        return
            ( "Function image-equals, FunctionAsRelation representation"
            , \ fresh ->
                let
                    (iPat, i) = quantifiedVar (fresh `at` 0)
                in
                    [essence| or([ &i[1] = &x /\ &i[2] <= &y | &iPat <- &rel ]) |]
            )

    -- >=
    theRule [essence| image(&func, &x) >= &y |] = do
        "FunctionAsRelation" <- representationOf func
        [rel]                <- downX1 func
        return
            ( "Function image-equals, FunctionAsRelation representation"
            , \ fresh ->
                let
                    (iPat, i) = quantifiedVar (fresh `at` 0)
                in
                    [essence| or([ &i[1] = &x /\ &i[2] >= &y | &iPat <- &rel ]) |]
            )
    theRule [essence| &y <= image(&func, &x) |] = do
        "FunctionAsRelation" <- representationOf func
        [rel]                <- downX1 func
        return
            ( "Function image-equals, FunctionAsRelation representation"
            , \ fresh ->
                let
                    (iPat, i) = quantifiedVar (fresh `at` 0)
                in
                    [essence| or([ &i[1] = &x /\ &i[2] >= &y | &iPat <- &rel ]) |]
            )

    theRule _ = na "rule_Image"


rule_Comprehension :: Rule
rule_Comprehension = "function-comprehension{FunctionAsRelation}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gofBefore, (pat, func), gofAfter) <- matchFirst gensOrConds $ \ gof -> case gof of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension"
        "FunctionAsRelation" <- representationOf func
        [rel]                <- downX1 func
        return
            ( "Mapping over a function, FunctionAsRelation representation"
            , const $
                Comprehension body
                    $  gofBefore
                    ++ [ Generator (GenInExpr pat rel) ]
                    ++ gofAfter
            )
    theRule _ = na "rule_Comprehension"
