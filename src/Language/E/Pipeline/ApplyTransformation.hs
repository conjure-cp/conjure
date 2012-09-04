{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}
{-# LANGUAGE QuasiQuotes #-}

module Language.E.Pipeline.ApplyTransformation ( applyTransformation ) where

import Language.E
import qualified Text.PrettyPrint as Pr

type RulesDB m = [E -> CompE m (Maybe [(String,E)])]

applyTransformation :: (Functor m, Monad m)
    => RulesDB m
    -> Spec
    -> CompE m Spec
applyTransformation fs spec = do
    mkLog "debug:ApplyTransformation.worker" $ pretty spec
    (spec', Any flag) <- runWriterT $ onSpec fs spec
    -- return spec'
    if flag
         then tryAgain fs spec'
         else returns []
         -- else return spec'
    -- return spec


tryAgain :: (Functor m, Monad m)
    => RulesDB m
    -> Spec
    -> CompE m Spec
-- tryAgain :: (Functor m, Monad m) => RuleBase (CompE m) -> Middleware (CompE m) Spec [Spec]
-- tryAgain _ spec = return [spec]
-- tryAgain fs spec = trace (show $ "tryAgain" <+> pretty spec) $ do
tryAgain fs spec = do
    modifyLocal $ \ st -> st { binders = [] }
    mkLog "debug:ApplyTransformation.tryAgain" $ pretty spec
    (result, Any flag) <- runWriterT $ onSpec fs spec
    if flag
        then tryAgain fs result
        else return spec


-- onSpec :: (Functor m, Monad m)
--     => RulesDB m
--     -> Spec
--     -> WriterT Any (CompE m) Spec
onSpec fs (Spec lang statements) = do
    lift $ mapM_ processStatement statements
    statements' <- sequence <$> mapM (onE fs) statements
    lift $ returns [ Spec lang s | s <- statements' ]


-- onE :: (Functor m, Monad m)
--     => RulesDB m
--     -> E
--     -> WriterT Any (CompE m) [E]
-- onE _ x | trace (show $ "onE" <+> pretty x) False = undefined
onE fs x@(Prim {}) = tryApply fs x
onE fs x@[xMatch| [Prim (S r)] := quantified.quanVar.structural.single.reference
                | xs           := quantified
                |] = do
-- onE fs x@( viewDeep [":expr-quantified"] -> Just xs ) = 
--     case lookUpInExpr ":expr-quantified-quanVar" xs of
--         Just [Expr ":structural-single" [R r]] -> do
            bindersBefore <- lift $ getsLocal binders
            let
                restoreState = do
                    bs1 <- getsLocal binders
                    modifyLocal $ \ st -> st { binders = bindersBefore }
                    bs2 <- getsLocal binders
                    mkLog "restoreState" $ vcat [ "before:" <+> prettyList id "," [ a | Binder a _ <- bs1 ]
                                                , "after :" <+> prettyList id "," [ a | Binder a _ <- bs2 ]
                                                ]
            -- lift $ mkLog " ------------ adding   quanVar ------------ " $ pretty r
            lift $ addBinder r
                    [xMake| quanVar.name   := [Prim (S r)]
                          | quanVar.within := [x]
                          |]
            result <- onEGeneric fs x "quantified" xs
            lift restoreState
            -- lift $ mkLog " ------------ removing quanVar ------------ " $ pretty r
            return result
onE fs [xMatch| [actual] := withLocals.actual
              | locals   := withLocals.locals
              |] = do
    bindersBefore <- lift $ getsLocal binders
    let
        restoreState = do
            bs1 <- getsLocal binders
            modifyLocal $ \ st -> st { binders = bindersBefore }
            bs2 <- getsLocal binders
            mkLog "restoreState" $ vcat [ "before:" <+> prettyList id "," [ a | Binder a _ <- bs1 ]
                                        , "after :" <+> prettyList id "," [ a | Binder a _ <- bs2 ]
                                        ]
    lift $ mapM_ processStatement locals
    results    <- onE fs actual
    outLocalss <- sequence <$> mapM (onE fs) locals 
    lift restoreState
    return [ [xMake| withLocals.actual := [result]
                   | withLocals.locals := outLocals
                   |]
           | result <- results
           , outLocals <- outLocalss
           ]
onE fs x@(Tagged t xs) = onEGeneric fs x t xs


-- onEGeneric :: (Functor m, Monad m)
--     => RulesDB m
--     -> E
--     -> String
--     -> [E]
--     -> WriterT Any (CompE m) [E]
onEGeneric fs x t xs = do
    (results, Any flag) <- listen $ do
        yss <- sequence <$> mapM (onE fs) xs
        let zs = map (Tagged t) yss
        concat <$> mapM (tryApply fs) zs
    if flag
        then concat <$> mapM (onE fs) results
        else return [x]

    -- (result, Any flag) <- listen $ do
    --     ys <- mapM (onE fs) xs
    --     let z = Expr t ys
    --     tryApply fs z
    -- if flag
    --     then return result
    --     else return [x]


-- tryApply :: (Functor m, Monad m)
--     => RulesDB m
--     -> E
--     -> WriterT Any (CompE m) [E]

tryApply fs x = do
    -- lift $ mkLog "debug" $ "in tryApply" <+> pretty x
    let
        -- returns a pair, first component: True if a modification has been made.
        --               , second component: list of results. should always be non-empty.
        go [] = return (False, [x])
        go (g:gs) = do
            mys <- g x
            case mys of
                Nothing -> go gs
                -- Nothing -> do
                --     case x of
                --         Tagged "quantified" _ -> mkLog "not applied" $ pretty x
                --         _ -> return ()
                --     go gs
                Just [] -> do
                    throwError (ErrFatal, "Rewrites to nothing.")
                Just ys -> do
                    mkLog "applied"
                        $ vcat $ pretty x
                               : [ Pr.braces (stringToDoc n) $$ nest 4 (pretty y) | (n,y) <- ys ]
                    -- mkLog "applied" $ vcat [pretty x, "~~>", pretty y]
                    return (True, map snd ys)
    (x', Any flag) <- listen (simplify x)
    if flag
        then do
            lift $ mkLog "simplified" $ vcat [pretty x, "~~>", pretty x']
            return [x']
        else do
            (b,z) <- lift $ go fs
            tell (Any b)
            return z

-- much needed
processStatement :: Monad m => E -> CompE m ()
processStatement s@[xMatch| [Prim (S name)] := topLevel.declaration.find.name.reference
                          | [      _      ] := topLevel.declaration.find.domain
                          |] = addBinder name s
processStatement s@[xMatch| [Prim (S name)] := topLevel.declaration.given.name.reference
                          | [      _      ] := topLevel.declaration.given.domain
                          |] = addBinder name s
processStatement   [xMatch| [Prim (S name)] := topLevel.declaration.letting.name.reference
                          | [ expression ]  := topLevel.declaration.letting.expr
                          |] = addBinder name expression
processStatement   [xMatch| _ := topLevel.suchThat  |] = return ()
processStatement   [xMatch| _ := topLevel.objective |] = return ()
processStatement   [xMatch| _ := topLevel.where     |] = return ()
processStatement s@[xMatch| _ := topLevel           |] = mkLog "processStatement" $ "not handled in processStatement" <+> prettyAsPaths s
processStatement s = mkLog "processStatement" $ "not handled in processStatement" <+> prettyAsPaths s
processStatement _ = return ()
