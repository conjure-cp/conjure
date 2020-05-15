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


-- TODO
-- rule_PowerSet_Comprehension :: Rule
-- rule_PowerSet_Comprehension = "function-powerSet-comprehension{FunctionAsRelation}" `namedRule` theRule where
--     theRule (Comprehension body gensOrConds) = do
--         traceM $ show $ "rule_PowerSet_Comprehension [1]" <+> pretty (Comprehension body gensOrConds)
--         (gocBefore, (pat, expr), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
--             Generator (GenInExpr pat expr) -> return (pat, expr)
--             _ -> na "rule_Comprehension"
--         traceM $ show $ "rule_PowerSet_Comprehension [2]" <+> pretty expr
--         func                  <- matchDefs [opToSet,opToMSet,opToRelation] <$> match opPowerSet expr
--         traceM $ show $ "rule_PowerSet_Comprehension [3]" <+> pretty func
--         repr <- representationOf func
--         traceM $ show $ "rule_PowerSet_Comprehension [4]" <+> pretty repr
--         Function_AsRelation{} <- representationOf func
--         traceM $ "rule_PowerSet_Comprehension [4]"
--         [rel]                 <- downX1 func
--         traceM $ show $ "rule_PowerSet_Comprehension [4]" <+> pretty rel
--         return
--             ( "Mapping over a function, FunctionAsRelation representation"
--             , return $
--                 Comprehension body
--                     $  gocBefore
--                     ++ [ Generator (GenInExpr pat (make opPowerSet rel)) ]
--                     ++ gocAfter
--             )
--     theRule _ = na "rule_PowerSet_Comprehension"
