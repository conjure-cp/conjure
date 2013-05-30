{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Pipeline.ApplyRefn ( applyRefn, applyRefnE ) where

import Conjure.Mode

import Language.E
import Language.E.BuiltIn
import qualified Language.E.Pipeline.ApplyRefnSlower as Slower ( applyRefn, applyRefnE )

import qualified Data.HashSet as S
import qualified Text.PrettyPrint as Pr


type RuleRefnDB m = [E -> m (Maybe [(Text, E)])]


applyRefn
    :: MonadConjureList m
    => RuleRefnDB m
    -> Spec
    -> m Spec
applyRefn db' spec = do
    ConjureModeWithFlags _ _ flags _ <- getsGlobal conjureMode
    if "--better" `S.member` flags
        then Slower.applyRefn db' spec
        else withBindingScope' $ do
            let db = db' ++ builtInRefn
            (spec', _) <- runWriterT $ onSpec db spec
            return spec'

applyRefnE
    :: MonadConjureList m
    => RuleRefnDB m
    -> E
    -> m E
applyRefnE db' x = do
    ConjureModeWithFlags _ _ flags _ <- getsGlobal conjureMode
    if "--better" `S.member` flags
        then Slower.applyRefnE db' x
        else return x


{-# INLINEABLE onSpec #-}
onSpec
    :: MonadConjureList m
    => RuleRefnDB m
    -> Spec
    -> WriterT Any m Spec
onSpec db (Spec lang statements) = Spec lang <$> onE db statements



{-# INLINEABLE onE #-}
onE
    :: MonadConjureList m
    => RuleRefnDB m
    -> E
    -> WriterT Any m E
-- onE _ x | trace (show $ "onE" <+> pretty x) False = undefined
onE = applyToTree



{-# INLINEABLE applyIdempotent #-}
applyIdempotent
    :: MonadConjureList m
    => RuleRefnDB m
    -> E
    -> WriterT Any m E
-- applyIdempotent _  x | trace (show $ "applyIdempotent" <+> pretty x) False = undefined
applyIdempotent db x = do
    (y, Any flag) <- listen $ apply db x
    if flag
        then applyToTree db y
        else return x



{-# INLINEABLE applyToTree #-}
applyToTree
    :: MonadConjureList m
    => RuleRefnDB m
    -> E
    -> WriterT Any m E
-- applyToTree _  x | trace (show $ "applyToTree" <+> pretty x) False = undefined
applyToTree db = bottomUpERefn 0 (applyIdempotent db)



-- apply refinement rules to x
-- doesn't bind or remove any bindings
-- modification (or not) info is carried in the writer state
{-# INLINEABLE apply #-}
apply
    :: MonadConjureList m
    => RuleRefnDB m
    -> E
    -> WriterT Any m E
-- apply _  x | trace (show $ "apply" <+> pretty x) False = undefined
apply db x = do
    mode <- lift $ getsGlobal conjureMode
    (ys, flag) <- lift $ tryApply db mode x
    tell (Any flag)
    lift $ returns ys



-- tryies to apply refinement rules to x
-- x is simplified first
-- doesn't descend or anything
-- results are simplified again after rule applications
{-# INLINEABLE tryApply #-}
tryApply
    :: ( MonadConjure m
       , RandomM m
       )
    => RuleRefnDB m
    -> ConjureModeWithFlags
    -> E
    -> m ([E], Bool)
-- tryApply db x = trace (show $ "tryApply:" <+> pretty x) $ do
tryApply db mode x = do
    (x' , b1) <- simply x
    when b1 $ mkLog "simplified" $ vcat [pretty x, "~~>", pretty x']
    (x'', b2) <- go db x'
    x''' <- selectByMode mode x''
    return (x''', b1 || b2)

    where

        simply :: MonadConjure m => E -> m (E, Bool)
        simply i = do
            (j, (Any flag, _)) <- runWriterT $ simplify i
            return (j, flag)

        -- returns a pair, first component: list of results. will always be non-empty.
        --               , second component: True if a modification has been made.
        go  :: MonadConjure m
            => RuleRefnDB m
            -> E
            -> m ([E], Bool)
        go []     i = return ([i], False)
        go (g:gs) i = do
            mys <- g i
            case mys of
                Nothing -> go gs i
                Just [] -> err ErrFatal "Rewrites to nothing."
                Just ys -> do
                    ys' <- forM ys $ \ (n,y) -> do (y', _) <- simply y ; return (n,y')
                    let msg = vcat $ pretty x
                               : [ "multiple:" <+> Pr.parens (pretty n) | let n = length ys', n > 1 ]
                              ++ [ Pr.braces (pretty n) $$ nest 4 (pretty y)
                                 | (n,y) <- ys'
                                 ]
                    mkLog "applied" msg
                    return (map snd ys', True)

