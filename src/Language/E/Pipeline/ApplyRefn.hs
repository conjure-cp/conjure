{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Pipeline.ApplyRefn ( applyRefn ) where

import Language.E
import Language.E.BuiltIn ( builtInRefn )

import qualified Text.PrettyPrint as Pr

type RulesDB m = [E -> m (Maybe [(String,E)])]


applyRefn
    :: ( MonadConjure m
       , MonadList m
       )
    => RulesDB m
    -> Spec
    -> m Spec
applyRefn db' spec = withBindingScope' $ do
    let db = db' ++ builtInRefn
    -- (spec', _) <- runWriterT $ (onSpec db >=> onSpec db) spec
    (spec', _) <- runWriterT $ onSpec db spec
    return spec'


{-# INLINE onSpec #-}
onSpec
    :: ( MonadConjure m
       , MonadList m
       )
    => RulesDB m
    -> Spec
    -> WriterT Any m Spec
onSpec db (Spec lang statements) = Spec lang <$> onE db statements



{-# INLINE onE #-}
onE
    :: ( MonadConjure m
       , MonadList m
       )
    => RulesDB m
    -> E
    -> WriterT Any m E
-- onE _ x | trace (show $ "onE" <+> pretty x) False = undefined
onE = applyToTree



{-# INLINE applyIdempotent #-}
applyIdempotent
    :: ( MonadConjure m
       , MonadList m
       )
    => RulesDB m
    -> E
    -> WriterT Any m E
-- applyIdempotent _  x | trace (show $ "applyIdempotent" <+> pretty x) False = undefined
applyIdempotent db x = do
    (y, Any flag) <- listen $ apply db x
    if flag
        then applyToTree db y
        else return y



{-# INLINE applyToTree #-}
applyToTree
    :: ( MonadConjure m
       , MonadList m
       )
    => RulesDB m
    -> E
    -> WriterT Any m E
-- applyToTree _  x | trace (show $ "applyToTree" <+> pretty x) False = undefined
applyToTree db = bottomUpE (applyIdempotent db)



-- apply refinement rules to x
-- doesn't bind or remove any bindings
-- modification (or not) info is carried in the writer state
{-# INLINE apply #-}
apply
    :: ( MonadConjure m
       , MonadList m
       )
    => RulesDB m
    -> E
    -> WriterT Any m E
-- apply _  x | trace (show $ "apply" <+> pretty x) False = undefined
apply db x = do
    (ys, flag) <- lift $ tryApply db x
    tell (Any flag)
    lift $ returns ys



-- tryies to apply refinement rules to x
-- x is simplified first
-- doesn't descend or anything
-- results are simplified again after rule applications
{-# INLINE tryApply #-}
tryApply
    :: MonadConjure m
    => RulesDB m
    -> E
    -> m ([E], Bool)
tryApply db x = do
    let
        simply :: MonadConjure m => E -> m (E, Bool)
        simply i = do
            (j, (Any flag, _)) <- runWriterT $ simplify i
            return (j, flag)
    let
        -- returns a pair, first component: True if a modification has been made.
        --               , second component: list of results. should always be non-empty.
        go  :: MonadConjure m
            => RulesDB m
            -> m ([E], Bool)
        go [] = return ([x], False)
        go (g:gs) = do
            mys <- g x
            case mys of
                Nothing -> go gs
                Just [] -> err ErrFatal "Rewrites to nothing."
                Just ys -> do
                    ys' <- forM ys $ \ (n,y) -> do (y', _) <- simply y ; return (n,y')
                    let msg = vcat $ pretty x
                               : [ Pr.braces (stringToDoc n) $$ nest 4 (pretty y)
                                 | (n,y) <- ys'
                                 ]
                    mkLog "applied" msg
                    -- trace (show $ "applied:" <+> msg) $ return (map snd ys', True)
                    return (map snd ys', True)
    (x', flag) <- simply x
    if flag
        then do
            mkLog "simplified" $ sep [pretty x, "~~>", pretty x']
            return ([x'], flag)
        else go db


