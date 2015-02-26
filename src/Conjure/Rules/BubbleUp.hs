{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.BubbleUp where

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


-- rule_MergeNested :: Rule
-- rule_MergeNested = "bubble-up-merge-nested" `namedRule` theRule where
--     -- theRule (WithLocals (WithLocals body locals1) locals2) =
--     --     return
--     --         ( "Merging nested bubbles"
--     --         , const $ WithLocals body (locals1 ++ locals2)
--     --         )
--     theRule _ = na "rule_MergeNested"
--
--
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
--
--
-- rule_ToAnd :: Rule
-- rule_ToAnd = "bubble-to-and" `namedRule` theRule where
--     theRule (WithLocals x []) = return ("Empty bubble is no bubble", const x)
--     theRule (WithLocals x locals) = do
--         TypeBool    <- typeOf x
--         (vars,cons) <- onlyConstraints locals
--         when (null cons) $ na "rule_ToAnd"
--         let out = make opAnd $ fromList (x:cons)
--         return
--             ( "Converting a bubble into a conjunction."
--             , const $ if null vars
--                         then out
--                         else WithLocals out vars
--             )
--     theRule _ = na "rule_BubbleToAnd"
--
--     onlyConstraints :: MonadFail m => [Statement] -> m ([Statement], [Expression])
--     onlyConstraints [] = return ([], [])
--     onlyConstraints (SuchThat xs:rest) = second (xs++) <$> onlyConstraints rest
--     onlyConstraints (decl:rest) = first (decl:) <$> onlyConstraints rest
--
--
-- rule_NotBoolYet :: Rule
-- rule_NotBoolYet = "bubble-up-NotBoolYet" `namedRule` theRule where
--     theRule Comprehension{} = na "rule_NotBoolYet Comprehension"
--     theRule WithLocals{}    = na "rule_NotBoolYet WithLocals"
--     theRule p = do
--         let
--             -- f x@(WithLocals y locals) = do
--             --     let decls = [ () | Declaration{} <- locals ]
--             --     ty <- typeOf y
--             --     case ty of
--             --         TypeBool                                 ->                return x
--             --         _        | length decls == length locals ->                return x     -- if all are decls
--             --         _                                        -> tell locals >> return y
--             f x = return x
--         (p', collected) <- runWriterT (descendM f p)
--         when (null collected) $
--             na "rule_NotBoolYet doesn't have any bubbly children"
--         return
--             ( "Bubbling up, not reached a relational context yet."
--             -- , const $ WithLocals p' collected
--             , error ""
--             )


rule_LiftVars :: Rule
rule_LiftVars = "bubble-up-LiftVars" `namedRule` theRule where
    theRule (Comprehension (WithLocals body locals@(_:_) []) gensOrConds) = do

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
                [ Comprehension c [Generator (GenDomainHasRepr patName indexDomain)]
                | c <- transformBi upd cons
                ]

        return
            ( "Bubbling up auxiliary variables through a comprehension."
            , const $ WithLocals
                         (Comprehension (transform upd body)
                             $  transformBi upd gocBefore
                             ++ [Generator (GenDomainHasRepr patName indexDomain)]
                             ++ transformBi upd gocAfter)
                          (declsLifted ++ [SuchThat consLifted])
                          []
            )
    theRule p = do
        let
            f x@(WithLocals y locals@(_:_) []) = do
                tell locals
                return y
            f x = return x
        (p', collected) <- runWriterT (descendM f p)
        when (null collected) $
            na "rule_LiftVars doesn't have any bubbly children"
        return
            ( "Bubbling up auxiliary variables."
            , const $ WithLocals p' collected []
            )
