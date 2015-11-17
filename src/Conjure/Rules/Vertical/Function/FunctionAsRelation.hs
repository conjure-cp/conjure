{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.Vertical.Function.FunctionAsRelation where

import Conjure.Rules.Import


-- TODO: this is incomplete
rule_Image_Eq :: Rule
rule_Image_Eq = "function-image-eq{FunctionAsRelation}" `namedRule` theRule where

    -- =
    theRule [essence| image(&func, &x) = &y |] = do
        Function_AsRelation{} <- representationOf func
        [rel]                 <- downX1 func
        return
            ( "Function image-equals, FunctionAsRelation representation"
            , do
                (iPat, i) <- quantifiedVar
                return [essence| or([ &i[1] = &x /\ &i[2] = &y | &iPat <- &rel ]) |]
            )
    theRule [essence| &y = image(&func, &x) |] = do
        Function_AsRelation{} <- representationOf func
        [rel]                 <- downX1 func
        return
            ( "Function image-equals, FunctionAsRelation representation"
            , do
                (iPat, i) <- quantifiedVar
                return [essence| or([ &i[1] = &x /\ &i[2] = &y | &iPat <- &rel ]) |]
            )

    -- <=
    theRule [essence| image(&func, &x) <= &y |] = do
        Function_AsRelation{} <- representationOf func
        [rel]                 <- downX1 func
        return
            ( "Function image-equals, FunctionAsRelation representation"
            , do
                (iPat, i) <- quantifiedVar
                return [essence| or([ &i[1] = &x /\ &i[2] <= &y | &iPat <- &rel ]) |]
            )
    theRule [essence| &y >= image(&func, &x) |] = do
        Function_AsRelation{} <- representationOf func
        [rel]                 <- downX1 func
        return
            ( "Function image-equals, FunctionAsRelation representation"
            , do
                (iPat, i) <- quantifiedVar
                return [essence| or([ &i[1] = &x /\ &i[2] <= &y | &iPat <- &rel ]) |]
            )

    -- >=
    theRule [essence| image(&func, &x) >= &y |] = do
        Function_AsRelation{} <- representationOf func
        [rel]                 <- downX1 func
        return
            ( "Function image-equals, FunctionAsRelation representation"
            , do
                (iPat, i) <- quantifiedVar
                return [essence| or([ &i[1] = &x /\ &i[2] >= &y | &iPat <- &rel ]) |]
            )
    theRule [essence| &y <= image(&func, &x) |] = do
        Function_AsRelation{} <- representationOf func
        [rel]                 <- downX1 func
        return
            ( "Function image-equals, FunctionAsRelation representation"
            , do
                (iPat, i) <- quantifiedVar
                return [essence| or([ &i[1] = &x /\ &i[2] >= &y | &iPat <- &rel ]) |]
            )

    theRule _ = na "rule_Image"


rule_Comprehension :: Rule
rule_Comprehension = "function-comprehension{FunctionAsRelation}" `namedRule` theRule where
    theRule (Comprehension body gensOrConds) = do
        (gocBefore, (pat, func), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenInExpr pat@Single{} expr) -> return (pat, matchDefs [opToSet,opToMSet,opToRelation] expr)
            _ -> na "rule_Comprehension"
        Function_AsRelation{} <- representationOf func
        [rel]                 <- downX1 func
        return
            ( "Mapping over a function, FunctionAsRelation representation"
            , return $
                Comprehension body
                    $  gocBefore
                    ++ [ Generator (GenInExpr pat rel) ]
                    ++ gocAfter
            )
    theRule _ = na "rule_Comprehension"
