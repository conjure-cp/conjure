{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.ApplyRefn ( applyRefn ) where

import Language.E
import Language.E.BuiltIn ( builtInRefn )

import qualified Data.Text as T
import qualified Text.PrettyPrint as Pr

type RulesDB m = [E -> CompE m (Maybe [(String,E)])]


applyRefn :: (Functor m, Monad m)
    => RulesDB m
    -> Spec
    -> CompE m Spec
applyRefn fs' spec = do
    let fs = fs' ++ builtInRefn
    (spec', Any flag) <- runWriterT $ onSpec fs spec
    if flag
         then tryAgain fs spec'
         else returns []


tryAgain :: (Functor m, Monad m)
    => RulesDB m
    -> Spec
    -> CompE m Spec
tryAgain fs spec = do
    modifyLocal $ \ st -> st { binders = [] }
    (result, Any flag) <- runWriterT $ onSpec fs spec
    if flag
        then tryAgain fs result
        else return spec


onSpec :: (Functor m, Monad m)
    => RulesDB m
    -> Spec
    -> WriterT Any (FunkyT LocalState GlobalState (CompError, Maybe Spec) m) Spec
onSpec fs (Spec lang statements) = do
    lift $ mapM_ processStatement statements
    statements' <- sequence <$> mapM (onE fs) statements
    lift $ returns [ Spec lang s | s <- statements' ]


onE :: (Functor m, Monad m)
    => RulesDB m
    -> E
    -> WriterT Any (FunkyT LocalState GlobalState (CompError, Maybe Spec) m) [E]
onE fs x@(Prim {}) = tryApply fs x
onE fs x@[xMatch| [Prim (S r)] := quantified.quanVar.structural.single.reference
                | xs           := quantified
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


onEGeneric :: (Functor m, Monad m)
    => RulesDB m
    -> E
    -> T.Text
    -> [E]
    -> WriterT Any (FunkyT LocalState GlobalState (CompError, Maybe Spec) m) [E]
onEGeneric fs x t xs = do
    (results, Any flag) <- listen $ do
        yss <- sequence <$> mapM (onE fs) xs
        let zs = map (Tagged t) yss
        concat <$> mapM (tryApply fs) zs
    if flag
        then concat <$> mapM (onE fs) results
        else return [x]


tryApply :: (Functor m, Monad m)
    => RulesDB m
    -> E
    -> WriterT Any (FunkyT LocalState GlobalState (CompError, Maybe Spec) m) [E]
tryApply fs x = do
    let
        -- returns a pair, first component: True if a modification has been made.
        --               , second component: list of results. should always be non-empty.
        go [] = return (False, [x])
        go (g:gs) = do
            mys <- g x
            case mys of
                Nothing -> go gs
                Just [] ->
                    err ErrFatal $ "Rewrites to nothing."
                Just ys -> do
                    ys' <- forM ys $ \ (n,y) -> do y' <- trySimplifyE y ; return (n,y')
                    mkLog "applied"
                        $ vcat $ pretty x
                               : [ Pr.braces (stringToDoc n) $$ nest 4 (pretty y) | (n,y) <- ys' ]

                    return (True, map snd ys')
    (x', Any flag) <- listen (simplify x)
    x'' <- lift $ trySimplifyE x'
    if flag
        then do
            lift $ mkLog "simplified" $ vcat [pretty x, "~~>", pretty x'']
            return [x'']
        else do
            (b,zs) <- lift $ go fs
            tell (Any b)
            return zs
