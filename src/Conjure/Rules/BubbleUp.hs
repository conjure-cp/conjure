{-# LANGUAGE QuasiQuotes #-}

module Conjure.Rules.BubbleUp where

import Conjure.Rules.Import


rule_MergeNested :: Rule
rule_MergeNested = "bubble-up-merge-nested" `namedRule` theRule where
    theRule (WithLocals (WithLocals body (DefinednessConstraints locals1)) (DefinednessConstraints locals2)) =
        return
            ( "Merging nested bubbles"
            , return $ WithLocals body (DefinednessConstraints (locals1 ++ locals2))
            )
    theRule (WithLocals (WithLocals body (AuxiliaryVars locals1)) (AuxiliaryVars locals2)) =
        return
            ( "Merging nested bubbles"
            , return $ WithLocals body (AuxiliaryVars (locals1 ++ locals2))
            )
    theRule _ = na "rule_MergeNested"


rule_ToAnd :: Rule
rule_ToAnd = "bubble-to-and" `namedRule` theRule where
    theRule (WithLocals x (AuxiliaryVars [])) = return ("Empty bubble is no bubble", return x)
    theRule (WithLocals x (DefinednessConstraints [])) = return ("Empty bubble is no bubble", return x)
    theRule (WithLocals x (DefinednessConstraints locals@(_:_))) = do
        TypeBool <- typeOf x
        let out = make opAnd $ fromList (x:locals)
        return
            ( "Converting a bubble into a conjunction."
            , return out
            )
    theRule _ = na "rule_BubbleToAnd"


rule_NotBoolYet :: Rule
rule_NotBoolYet = "bubble-up-NotBoolYet" `namedRule` theRule where
    theRule WithLocals{}    = na "rule_NotBoolYet WithLocals"

    -- if anything in a comprehension is undefined, the whole comprehension is undefined
    -- this is for the non-bool case.
    theRule (Comprehension (WithLocals body (DefinednessConstraints locals@(_:_))) gensOrConds) = do

        ty <- typeOf body
        case ty of
            TypeBool -> na "rule_NotBoolYet"
            _        -> return ()

        forM_ gensOrConds $ \ goc -> case goc of
            Generator GenDomainHasRepr{} -> return ()
            Generator {}                 -> na "rule_NotBoolYet"        -- no other generators, only GenDomainHasRepr
            Condition {}                 -> return ()
            ComprehensionLetting {}      -> na "rule_NotBoolYet"        -- no lettings

        let localsLifted =
                [ make opAnd $ Comprehension c gensOrConds
                | c <- locals
                ]

        return
            ( "Bubbling up (through comprehension), not reached a relational context yet."
            , return $ WithLocals (Comprehension body gensOrConds) (DefinednessConstraints localsLifted)
            )

    theRule Comprehension{} = na "rule_NotBoolYet Comprehension"

    theRule p = do
        let
            f x@(WithLocals y (DefinednessConstraints locals@(_:_))) = do
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
            , return $ WithLocals p' (DefinednessConstraints collected)
            )


rule_LiftVars :: Rule
rule_LiftVars = "bubble-up-LiftVars" `namedRule` theRule where

    theRule (Comprehension (WithLocals body locals) gensOrConds)
        | and [ case goc of
                    Condition{} -> True
                    ComprehensionLetting{} -> True
                    _ -> False
              | goc <- gensOrConds
              ]                                                 -- if gensOrConds do not contain generators
        = return
            ( "Bubbling up when a comprehension doesn't contain any generators."
            , return $ WithLocals (Comprehension body gensOrConds) locals
            )

    theRule (Comprehension (WithLocals body (AuxiliaryVars locals@(_:_))) gensOrConds) = do

        let decls = [ (nm,dom) | Declaration (FindOrGiven LocalFind nm dom) <- locals ]
        let cons  = concat [ xs | SuchThat xs <- locals ]

        -- TODO: what to do with the conditions?
        -- should we `dontCare unless condition`?
        -- discard for now
        (_conditions, generators) <- fmap mconcat $ forM gensOrConds $ \ goc -> case goc of
            Condition{} -> return ([goc], [])
            ComprehensionLetting{} -> return ([goc], [])
            Generator (GenDomainHasRepr patName domain) -> return ([], [(patName, domain)])
            _ -> na "rule_LiftVars"


        let patRefs = [ Reference patName Nothing | (patName, _) <- generators ]
        let indexDomains = map snd generators

        let upd (Reference nm _) | nm `elem` map fst decls
                = let r = Reference nm Nothing
                  in  make opMatrixIndexing r patRefs
            upd r = r

        let declsLifted =
                [ Declaration (FindOrGiven LocalFind nm domLifted)
                | (nm, dom) <- decls
                , let domLifted = foldr (\ i j -> DomainMatrix (forgetRepr i) j ) dom indexDomains
                ]

        let consLifted =
                [ make opAnd $ Comprehension c [Generator (GenDomainHasRepr n d) | (n,d) <- generators]
                | c <- transformBi upd cons
                ]

        return
            ( "Bubbling up auxiliary variables through a comprehension."
            , return $ WithLocals (Comprehension (transform upd body) (transformBi upd gensOrConds))
                                  (AuxiliaryVars (declsLifted ++ [SuchThat consLifted]))
            )
    theRule WithLocals{} = na "rule_LiftVars"
    theRule p = do
        let
            f (WithLocals y (AuxiliaryVars locals@(_:_))) = do
                tell locals
                return y
            f x = return x
        (p', collected) <- runWriterT (descendM f p)
        when (null collected) $
            na "rule_LiftVars doesn't have any bubbly children"
        return
            ( "Bubbling up auxiliary variables."
            , return $ WithLocals p' (AuxiliaryVars collected)
            )
