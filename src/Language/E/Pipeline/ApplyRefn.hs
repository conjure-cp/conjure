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
    initialiseSpecState spec
    (result, Any flag) <- runWriterT $ onSpec fs spec
    if flag
        then tryAgain fs result
        else return spec


onSpec :: (Functor m, Monad m)
    => RulesDB m
    -> Spec
    -> WriterT Any (FunkyT LocalState GlobalState (CompError, Maybe Spec) m) Spec
onSpec fs (Spec lang statements) = do
    bindersBefore <- lift $ getsLocal binders
    lift $ mapM_ introduceStuff statements
    statements' <- sequence <$> mapM (onE fs) statements
    lift $ modifyLocal $ \ st -> st { binders = bindersBefore }
    lift $ returns [ Spec lang s | s <- statements' ]



onE :: (Functor m, Monad m)
    => RulesDB m
    -> E
    -> WriterT Any (CompEMOnly m) [E]
-- onE _ x | trace (show $ "onE" <+> pretty x) False = undefined
onE fs x@(Prim {}) = tryApply' fs x
onE fs x@(Tagged t xs) = do
    bindersBefore <- lift $ getsLocal binders
    lift $ introduceStuff x
    results <- onEGeneric fs x t xs
    lift $ modifyLocal $ \ st -> st { binders = bindersBefore }
    return results


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
        concat <$> mapM (tryApply' fs) zs
    if flag
        then concat <$> mapM (onE fs) results
        else return [x]

tryApply' :: (Functor m, Monad m)
    => RulesDB m
    -> E
    -> WriterT Any (CompEMOnly m) [E]
tryApply' fx x = do
    (y, (z, _)) <- lift $ runWriterT (tryApply fx x)
    tell z
    return y

tryApply :: (Functor m, Monad m)
    => RulesDB m
    -> E
    -> WriterT (Any, [Binder]) (CompEMOnly m) [E]
tryApply fs x = do
    let
        -- returns a pair, first component: True if a modification has been made.
        --               , second component: list of results. should always be non-empty.
        go [] = return (False, [x])
        go (g:gs) = do
            mys <- g x
            case mys of
                Nothing -> go gs
                Just [] -> err ErrFatal "Rewrites to nothing."
                Just ys -> do
                    ys' <- forM ys $ \ (n,y) -> do (y', _) <- trySimplifyE y ; return (n,y')
                    let msg = vcat $ pretty x
                               : [ Pr.braces (stringToDoc n) $$ nest 4 (pretty y) | (n,y) <- ys' ]
                    mkLog "applied" msg
                    trace (show $ "applied" <+> msg) $ return (True, map snd ys')
                    -- return (True, map snd ys')
    (x', (Any flag, _)) <- listen (simplify x)
    (x'', _) <- lift $ trySimplifyE x'
    if flag
        then do
            lift $ mkLog "simplified" $ vcat [pretty x, "~~>", pretty x'']
            tell (Any True, [])
            return [x'']
        else do
            (b,zs) <- lift $ go fs
            tell (Any b, [])
            return zs


