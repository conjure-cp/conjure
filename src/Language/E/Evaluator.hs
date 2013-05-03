{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Language.E.Evaluator
    ( simplify
    , simplifySpec
    , fullySimplify
    , fullySimplifySpec
    ) where

import Stuff.Pretty

import Language.E.Imports
import Language.E.Definition
import Language.E.CompE
import Language.E.Traversals
import Language.E.Evaluator.Full    ( fullEvaluator
                                    , evalHasType, evalHasDomain
                                    , evalHasRepr, evalDomSize
                                    , evalIndices, evalReplace
                                    , tupleEq, matrixEq
                                    , stripStructuralSingle
                                    , stripUnnecessaryTyped
                                    , unrollQuantifiers
                                    )
import Language.E.Evaluator.Partial ( partialEvaluator )


-- _testSimplify :: T.Text -> IO ()
-- _testSimplify t = do
    -- let res = runLexerAndParser (inCompleteFile parseExpr) "" t
    -- case res of
        -- Left  e -> print e
        -- Right x -> do
            -- print $ pretty x
            -- ys <- runCompEIO $ runWriterT $ simplify x
            -- mapM_ (print . pretty . fst) ys


simplifySpec :: MonadConjure m => Spec -> m Spec
simplifySpec = liftM fst . runWriterT . bottomUpSpec simplify

fullySimplifySpec :: MonadConjure m => Spec -> m Spec
fullySimplifySpec = liftM fst . runWriterT . bottomUpSpec fullySimplify

fullySimplify :: MonadConjure m => E -> WriterT (Any, [Binder]) m E
fullySimplify x = do
    (y, (Any flag, _)) <- listen $ pipeline x
    if flag
        then fullySimplify y
        else return x
    where
        pipeline =  bottomUpE (mkIdempotent allCombinedDoFirst)
                >=> bottomUpE (mkIdempotent unrolling)

        unrolling :: MonadConjure m => E -> WriterT (Any, [Binder]) m E
        -- allCombined i = trace (show $ "allCombined:" <+> pretty i) $
        unrolling i =
            firstJustOr i
                $ map ($ i) [ logged "Evaluator"                        fullEvaluator
                            , logged "Evaluator.hasRepr"                evalHasRepr
                            , logged "Evaluator.hasType"                evalHasType
                            , logged "Evaluator.hasDomain"              evalHasDomain
                            , logged "Evaluator.domSize"                evalDomSize
                            , logged "Evaluator.indices"                evalIndices
                            , logged "Evaluator.replace"                evalReplace
                            , logged "Evaluator.tupleEq"                tupleEq
                            , logged "Evaluator.matrixEq"               matrixEq
                            , logged "Evaluator.stripStructuralSingle"  stripStructuralSingle
                            , logged "Simplify"                         (adapter partialEvaluator)
                            , logged "Evaluator.stripUnnecessaryTyped"  stripUnnecessaryTyped
                            , logged "Evaluator.unrollQuantifiers"      unrollQuantifiers
                            , logged "Evaluator.instantiate"            instantiate
                            ]

        instantiate [xMatch| [Prim (S nm)] := reference |]
            | nm `elem` ["_", "forAll", "exists", "sum"]
            = return Nothing
        instantiate [xMatch| [Prim (S nm)] := reference |] = do
            mres <- runMaybeT $ lookupReference nm
            return $ case mres of
                Nothing                         -> Nothing
                Just [xMatch| _ := quanVar |]   -> Nothing
                Just i                          -> Just (i, [])
        instantiate _ = return Nothing


simplify :: MonadConjure m => E -> WriterT (Any, [Binder]) m E
-- simplify x = trace (show $ "simplify" <+> pretty x) $ do
simplify x = do
    (y, (Any flag, _)) <- listen $ pipeline x
    if flag
        then simplify y
        else return x
    where
        pipeline =  bottomUpE (mkIdempotent allCombinedDoFirst)
                >=> bottomUpE (mkIdempotent allCombined)


allCombined :: MonadConjure m => E -> WriterT (Any, [Binder]) m E
-- allCombined i = trace (show $ "allCombined:" <+> pretty i) $
allCombined i =
    firstJustOr i
        $ map ($ i) [ logged "Evaluator"                        fullEvaluator
                    , logged "Evaluator.hasRepr"                evalHasRepr
                    , logged "Evaluator.hasType"                evalHasType
                    , logged "Evaluator.hasDomain"              evalHasDomain
                    , logged "Evaluator.domSize"                evalDomSize
                    , logged "Evaluator.indices"                evalIndices
                    , logged "Evaluator.replace"                evalReplace
                    , logged "Evaluator.tupleEq"                tupleEq
                    , logged "Evaluator.matrixEq"               matrixEq
                    , logged "Evaluator.stripStructuralSingle"  stripStructuralSingle
                    , logged "Simplify"                         (adapter partialEvaluator)
                    , logged "Evaluator.stripUnnecessaryTyped"  stripUnnecessaryTyped
                    ]

-- these transformations should be applied first. others might depend on them.
allCombinedDoFirst :: MonadConjure m => E -> WriterT (Any, [Binder]) m E
-- allCombinedDoFirst i = trace (show $ "allCombinedDoFirst:" <+> pretty i) $
allCombinedDoFirst i =
    firstJustOr i
        $ map ($ i) [ logged "Evaluator"           fullEvaluator
                    , logged "Evaluator.replace"   evalReplace
                    ]

mkIdempotent :: MonadConjure m
             => (E -> WriterT (Any, [Binder]) m E)
             ->  E -> WriterT (Any, [Binder]) m E
mkIdempotent f i = do
    (i', (Any flag, _)) <- listen $ f i
    if flag
        then mkIdempotent f i'
        else return i

adapter
    :: MonadConjure m
    => (E -> m (Maybe E))
    -> E
    -> m (Maybe (E, [Binder]))
adapter comp x = do
    y <- comp x
    case y of
        Nothing -> return Nothing
        Just z  -> return (Just (z, []))

logged
    :: MonadConjure m
    => String
    -> (E -> m (Maybe (E,[Binder])))
    -> E
    -> WriterT (Any, [Binder]) m (Maybe E)
-- logged str act inp = trace (show $ pretty str <+> pretty inp) $ do
logged str act inp = do
    moutp <- lift $ act inp
    case moutp of
        Nothing         -> return Nothing
        Just (outp, bs) -> do
            lift $ mkLog str $ sep [pretty inp, "~~>", pretty outp]
            tell (Any True, bs)
            return (Just outp)


firstJustOr
    :: Monad m
    => a
    -> [m (Maybe a)]
    -> m a
firstJustOr i []     = return i
firstJustOr i (a:as) = do
    mr <- a
    case mr of
        Nothing -> firstJustOr i as
        Just r  -> return r

