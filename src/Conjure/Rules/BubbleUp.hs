{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.BubbleUp where

import Conjure.Rules.Import


rule_MergeNested :: Rule
rule_MergeNested = "bubble-up-merge-nested" `namedRule` theRule where
    theRule (WithLocals (WithLocals body (Right locals1)) (Right locals2)) =
        return
            ( "Merging nested bubbles"
            , const $ WithLocals body (Right (locals1 ++ locals2))
            )
    theRule _ = na "rule_MergeNested"


-- rule_Comprehension :: Rule
-- rule_Comprehension = "bubble-up-comprehension" `namedRule` theRule where
--     -- theRule (Comprehension body gensOrConds) = do
--     --     (gocBefore, (pat, expr, locals), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
--     --         Generator (GenInExpr pat@Single{} (WithLocals expr locals)) -> return (pat, expr, locals)
--     --         _ -> na "rule_Comprehension"
--     --     locals' <- forM locals $ \ l -> case l of
--     --         SuchThat xs -> return xs
--     --         _ -> fail ("rule_Comprehension, not a SuchThat:" <+> pretty l)
--     --     return
--     --         ( "Bubble in the generator of a comprehension."
--     --         , const $ Comprehension body
--     --             $  gocBefore
--     --             ++ [Generator (GenInExpr pat expr)]
--     --             ++ map Condition (concat locals')
--     --             ++ gocAfter
--     --         )
--     theRule _ = na "rule_Comprehension"
--
--
-- rule_VarDecl :: Rule
-- rule_VarDecl = "bubble-up-VarDecl" `namedRule` theRule where
--     theRule Comprehension{} = na "rule_VarDecl Comprehension"
--     theRule WithLocals{}    = na "rule_VarDecl WithLocals"
--     theRule p = do
--         let
--             -- f x@(WithLocals y locals) = do
--             --     let decls = [ decl | decl@Declaration{} <- locals ]
--             --     if length decls == length locals
--             --         then tell decls >> return y         -- no cons, all decls
--             --         else               return x
--             f x = return x
--         (p', collected) <- runWriterT (descendM f p)
--         when (null collected) $
--             na "rule_VarDecl doesn't have any bubbly children"
--         return
--             ( "Bubbling up only declarations, no constraints in the bubble."
--             , const $ WithLocals p' collected []
--             )
--
--
-- rule_LocalInComprehension :: Rule
-- rule_LocalInComprehension = "bubble-up-local-in-comprehension" `namedRule` theRule where
--     theRule p = do
--         -- (mkQuan, Comprehension body gensOrConds) <- match opQuantifier p
--         -- (gocBefore, (pat, expr, locals), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
--         --     Generator (GenInExpr pat@Single{} (WithLocals expr locals)) -> return (pat, expr, locals)
--         --     _ -> na "rule_Comprehension"
--         -- return
--         --     ( "Bubble in the generator of a comprehension."
--         --     , const $ WithLocals
--         --         ( mkQuan $ Comprehension body
--         --             $  gocBefore
--         --             ++ [Generator (GenInExpr pat expr)]
--         --             ++ gocAfter
--         --         )
--         --         locals
--         --     )


rule_ToAnd :: Rule
rule_ToAnd = "bubble-to-and" `namedRule` theRule where
    theRule (WithLocals x (Left  [])) = return ("Empty bubble is no bubble", const x)
    theRule (WithLocals x (Right [])) = return ("Empty bubble is no bubble", const x)
    theRule (WithLocals x (Right locals@(_:_))) = do
        TypeBool <- typeOf x
        let out = make opAnd $ fromList (x:locals)
        return
            ( "Converting a bubble into a conjunction."
            , const out
            )
    theRule _ = na "rule_BubbleToAnd"


rule_NotBoolYet :: Rule
rule_NotBoolYet = "bubble-up-NotBoolYet" `namedRule` theRule where
    theRule WithLocals{}    = na "rule_NotBoolYet WithLocals"

    -- if anything in a comprehension is undefined, the whole comprehension is undefined
    -- this is for the non-bool case.
    theRule (Comprehension (WithLocals body (Right locals@(_:_))) gensOrConds) = do

        ty <- typeOf body
        case ty of
            TypeBool -> na "rule_NotBoolYet"
            _        -> return ()

        forM_ gensOrConds $ \ goc -> case goc of
            Generator GenDomainHasRepr{} -> return ()
            Generator {}                 -> na "rule_NotBoolYet"        -- no other generators
            Condition {}                 -> return ()

        let localsLifted =
                [ make opAnd $ Comprehension c gensOrConds
                | c <- locals
                ]

        return
            ( "Bubbling up (through comprehension), not reached a relational context yet."
            , const $ WithLocals (Comprehension body gensOrConds) (Right localsLifted)
            )
        
    theRule p = do
        let
            f x@(WithLocals y (Right locals@(_:_))) = do
                ty <- typeOf y
                case ty of
                    TypeBool ->                return x         -- do not bubble-up if it is attached to a bool
                    _        -> tell locals >> return y
            f x = return x
        (p', collected) <- runWriterT (descendM f p)
        when (null collected) $
            na "rule_NotBoolYet doesn't have any bubbly children"
        return
            ( "Bubbling up, not reached a relational context yet."
            , const $ WithLocals p' (Right collected)
            )


rule_LiftVars :: Rule
rule_LiftVars = "bubble-up-LiftVars" `namedRule` theRule where
    theRule (Comprehension (WithLocals body (Left locals@(_:_))) gensOrConds) = do

        let decls = [ (nm,dom) | Declaration (FindOrGiven LocalFind nm dom) <- locals ]
        let cons  = concat [ xs | SuchThat xs <- locals ]

        (gocBefore, (patName, indexDomain), gocAfter) <- matchFirst gensOrConds $ \ goc -> case goc of
            Generator (GenDomainHasRepr patName domain) -> return (patName, domain)
            _ -> na "rule_LiftVars"

        let patRef = Reference patName Nothing

        let upd (Reference nm _) | nm `elem` map fst decls
                = let r = Reference nm Nothing
                  in  [essence| &r[&patRef] |]
            upd r = r

        let declsLifted =
                [ Declaration (FindOrGiven LocalFind nm domLifted)
                | (nm, dom) <- decls
                , let domLifted = DomainMatrix (forgetRepr indexDomain) dom
                ]

        let consLifted =
                [ make opAnd $ Comprehension c [Generator (GenDomainHasRepr patName indexDomain)]
                | c <- transformBi upd cons
                ]

        return
            ( "Bubbling up auxiliary variables through a comprehension."
            , const $ WithLocals
                         (Comprehension (transform upd body)
                             $  transformBi upd gocBefore
                             ++ [Generator (GenDomainHasRepr patName indexDomain)]
                             ++ transformBi upd gocAfter)
                          (Left (declsLifted ++ [SuchThat consLifted]))
            )
    theRule WithLocals{} = na "rule_LiftVars"
    theRule p = do
        let
            f (WithLocals y (Left locals@(_:_))) = do
                tell locals
                return y
            f x = return x
        (p', collected) <- runWriterT (descendM f p)
        when (null collected) $
            na "rule_LiftVars doesn't have any bubbly children"
        return
            ( "Bubbling up auxiliary variables."
            , const $ WithLocals p' (Left collected)
            )
