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


rule_InDefined :: Rule
rule_InDefined = "function-inDefined{FunctionAsRelation}" `namedRule` theRule where
    theRule [essence| &x in defined(&func) |] = do
        TypeFunction{} <- typeOf func
        Function_AsRelation (Relation_AsSet Set_Explicit) <- representationOf func
        tableCheck x func
        xParts <- downX1 x
        let vars = fromList xParts
        [rel] <- downX1 func
        [set] <- downX1 rel
        [matrix] <- downX1 set
        [from, _to] <- downX1 matrix
        parts <- downX1 from
        (index:_) <- indexDomainsOf from
        (iPat, i) <- quantifiedVar
        let oneRow = fromList [ [essence| &p[&i] |] | p <- parts ]
        let table = [essence| [ &oneRow | &iPat : &index ] |]
        return
            ( "relation membership to table"
            , return [essence| table(&vars, &table) |]
            )
    theRule _ = na "rule_InDefined"

    tableCheck ::
        MonadFailDoc m =>
        (?typeCheckerMode::TypeCheckerMode) =>
        Expression -> Expression -> m ()
    tableCheck x func | categoryOf func < CatDecision = do
        tyX <- typeOf x
        case tyX of
            TypeTuple ts | and [ case t of TypeInt{} -> True ; _ -> False | t <- ts ] -> return ()
            _ -> na "tableCheck"
    tableCheck _ _ = na "tableCheck"


rule_InToSet :: Rule
rule_InToSet = "function-inToSet{FunctionAsRelation}" `namedRule` theRule where
    theRule [essence| &x in toSet(&func) |] = do
        TypeFunction{} <- typeOf func
        Function_AsRelation (Relation_AsSet Set_Explicit) <- representationOf func
        tableCheck x func
        [keyFrom, keyTo] <- downX1 x
        keyFromParts <- downX1 keyFrom
        let vars = fromList (keyFromParts ++ [keyTo])
        [rel] <- downX1 func
        [set] <- downX1 rel
        [matrix] <- downX1 set
        [from, to] <- downX1 matrix
        parts <- downX1 from
        (index:_) <- indexDomainsOf from
        (iPat, i) <- quantifiedVar
        let oneRow = fromList $ [ [essence| &p[&i] |] | p <- parts ]
                             ++ [ [essence| &to[&i] |] ]
        let table = [essence| [ &oneRow | &iPat : &index ] |]
        return
            ( "relation membership to table"
            , return [essence| table(&vars, &table) |]
            )
    theRule _ = na "rule_InToSet"

    tableCheck ::
        MonadFailDoc m =>
        (?typeCheckerMode::TypeCheckerMode) =>
        Expression -> Expression -> m ()
    tableCheck x func | categoryOf func < CatDecision = do
        tyX <- typeOf x
        case tyX of
            TypeTuple [TypeTuple ts, to] | and [ case t of TypeInt{} -> True ; _ -> False | t <- (to:ts) ] -> return ()
            _ -> na "tableCheck"
    tableCheck _ _ = na "tableCheck"
