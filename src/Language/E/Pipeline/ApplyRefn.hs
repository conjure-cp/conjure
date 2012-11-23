{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Pipeline.ApplyRefn ( applyRefn ) where

import Language.E
import Language.E.BuiltIn

import qualified Text.PrettyPrint as Pr

type RulesDB m = [E -> m (Maybe [(Text, E)])]


applyRefn
    :: ( MonadConjure m
       , MonadList m
       )
    => RulesDB m
    -> Spec
    -> m Spec
applyRefn db' spec = withBindingScope' $ do
    let db = db' ++ builtInRefn
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
-- tryApply db x = trace (show $ "tryApply:" <+> pretty x) $ do
tryApply db x = do
    (x' , b1) <- simply x
    when b1 $ mkLog "simplified" $ vcat [pretty x, "~~>", pretty x']
    (x'', b2) <- go db x'
    return (x'', b1 || b2)

    where

        simply :: MonadConjure m => E -> m (E, Bool)
        simply i = do
            (j, (Any flag, _)) <- runWriterT $ simplify i
            return (j, flag)

        -- returns a pair, first component: list of results. will always be non-empty.
        --               , second component: True if a modification has been made.
        go  :: MonadConjure m
            => RulesDB m
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
                               : [ Pr.braces (pretty n) $$ nest 4 (pretty y)
                                 | (n,y) <- ys'
                                 ]
                    mkLog "applied" msg
                    return (map snd ys', True)



_applyRefnTest2 :: Text -> IO ()
_applyRefnTest2 inp =
    case runLexerAndParser (inCompleteFile parseExpr) "in memory" inp of
        Left  x -> error $ show x
        Right x -> do
            print $ prettyAsPaths x
            let (mys, _logs)
                    = afterCompERun "foo"
                    $ runIdentity
                    $ runFunkySingle def
                    $ tryApply [_plusminus1] x
            -- printLogs logs
            case mys of
                Left  y -> error $ renderPretty y
                Right (ys,flag) -> do
                    print flag
                    forM_ ys $ \ y -> do
                        print $ prettyAsPaths y
                        print $ pretty y

_applyRefnTest3 :: Text -> IO ()
_applyRefnTest3 inp =
    case runLexerAndParser (inCompleteFile parseExpr) "in memory" inp of
        Left  x -> error $ show x
        Right x -> do
            -- print $ prettyAsPaths x
            let mys
                    = map (afterCompERun "foo")
                    $ fst
                    $ runIdentity
                    $ runFunkyMulti () def
                    $ runWriterT (onE [_aEqtoFoo, _aFooTo12] x)
            forM_ mys $ \ (my, _logs) -> do
                -- printLogs logs
                case my of
                    Left  y -> error $ renderPretty y
                    Right (y, _flag) -> do
                        -- print flag
                        -- print $ prettyAsPaths y
                        print $ pretty y

_applyRefnTest4 :: Text -> IO ()
_applyRefnTest4 inp =
    case runLexerAndParser (inCompleteFile parseExpr) "in memory" inp of
        Left  x -> error $ show x
        Right x -> do
            -- print $ prettyAsPaths x
            let mys
                    = map (afterCompERun "foo")
                    $ fst
                    $ runIdentity
                    $ runFunkyMulti () def
                    $ runWriterT (onE [_aBarTo12, _aEqtoFoo, _aFooTo12] x)
            forM_ mys $ \ (my, _logs) -> do
                -- printLogs logs
                case my of
                    Left  y -> error $ renderPretty y
                    Right (y, _flag) -> do
                        -- print flag
                        -- print $ prettyAsPaths y
                        print $ pretty y

_applyRefnMain :: IO ()
_applyRefnMain = _applyRefnTest4 "blah(blah(blah(a,b),blah(c,d)),e)"

_plusminus1 :: MonadConjure m => RefnFunc m
_plusminus1 [xMatch| [Prim (I i)] := value.literal |]
    = return $ Just [ ("_plusminus1-", [xMake| value.literal := [Prim (I $ i - 1)] |] )
                    , ("_plusminus1+", [xMake| value.literal := [Prim (I $ i + 1)] |] )
                    ]
_plusminus1 _ = return Nothing

_aEqtoFoo :: MonadConjure m => RefnFunc m
_aEqtoFoo [eMatch| blah(&a,&b) |]
    = return $ Just $ map (\ i -> ("_aEqtoFoo", i) ) [ [eMake| foo(&a,&b) |]
                                                     , [eMake| bar(&a,&b) |]
                                                     ]
_aEqtoFoo _ = return Nothing


_aFooTo12 :: MonadConjure m => RefnFunc m
_aFooTo12 [eMatch| foo(&a,&b) |]
    = return $ Just $ map (\ i -> ("_aFooTo12", i) ) [ [eMake| foo1(&a,&b) |]
                                                     , [eMake| foo2(&a,&b) |]
                                                     ]
_aFooTo12 _ = return Nothing


_aBarTo12 :: MonadConjure m => RefnFunc m
_aBarTo12 [eMatch| bar(&a,&b) |]
    = return $ Just $ map (\ i -> ("_aFooTo12", i) ) [ [eMake| bar1(&a,&b) |]
                                                     , [eMake| bar2(&a,&b) |]
                                                     ]
_aBarTo12 _ = return Nothing


