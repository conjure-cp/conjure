{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts#-}

module Language.E.Testing.TopToBottom where

-- given a list of repr rules,
--       a list of refn rules, and
--       a list of essence specifications.
-- 
-- parse each file repr & refn file.
-- parse each spec. pretty-print to inputName.parsed.
--                  parse inputName.parsed,
--                  compare with the original.
-- 
-- for each essence specification
--       run conjure-all

import Language.E
import Language.E.Pipeline.ReadIn

import Test.Hspec.Monadic (describe, it)
import qualified Test.Hspec.Monadic (Spec)



tests :: IO Test.Hspec.Monadic.Spec
tests = do
    ts <- iotests
    return $ describe "top to bottom" $ mapM_ (uncurry it) ts


iotests :: IO [(String, Bool)]
iotests = do
    ((_refns, _reprs, _specs), pairs) <- runWriterT $ do
        refns <- getAllRefns
        reprs <- getAllReprs
        specs <- getAllSpecs
        return (refns, reprs, specs)
    return pairs


getAllRefns
    :: (MonadWriter [(String, Bool)] m, MonadIO m)
    => m [RuleRefn]
getAllRefns = do
    gen      <- liftIO getStdGen
    files    <- liftIO $ allFilesWithSuffix ".rule" "files/rules"
    contents <- liftIO $ mapM pairWithContents files
    liftM (concat . catMaybes) $ forM contents $ \ pair@(fp,_) ->
        let
            (mxs, _) = runIdentity $ runCompE gen (readRuleRefn pair)
            errors   = lefts $ map fst mxs
        in  case mxs of
                [(Right a, _)] -> testSuccess a $ "parsing -- " ++ fp
                _              -> testFail $ "parsing -- " ++ fp ++ " -- "
                                          ++ renderPretty (prettyErrors "Parsing." errors)


getAllReprs
    :: (MonadWriter [(String, Bool)] m, MonadIO m)
    => m [RuleRepr]
getAllReprs = do
    gen      <- liftIO getStdGen
    files    <- liftIO $ allFilesWithSuffix ".repr" "files/rules"
    contents <- liftIO $ mapM pairWithContents files
    liftM catMaybes $ forM contents $ \ pair@(fp,_) ->
        let
            (mxs, _) = runIdentity $ runCompE gen (readRuleRepr pair)
            errors   = lefts $ map fst mxs
        in  case mxs of
                [(Right a, _)] -> testSuccess a $ "parsing -- " ++ fp
                _              -> testFail $ "parsing -- " ++ fp ++ " -- "
                                          ++ renderPretty (prettyErrors "Parsing." errors)


getAllSpecs
    :: (MonadWriter [(String, Bool)] m, MonadIO m)
    => m [(FilePath, Spec)]
getAllSpecs = do
    gen      <- liftIO getStdGen
    files    <- liftIO $ allFilesWithSuffix ".essence" "files/testdata"
    contents <- liftIO $ mapM pairWithContents files
    liftM catMaybes $ forM contents $ \ pair@(fp,_) ->
        let
            (mxs, _) = runIdentity $ runCompE gen (readSpec pair)
            errors   = lefts $ map fst mxs
        in  case mxs of
                [(Right a, _)] -> testSuccess (fp,a) $ "parsing -- " ++ fp
                _              -> testFail $ "parsing -- " ++ fp ++ " -- "
                                          ++ renderPretty (prettyErrors "Parsing." errors)


testSuccess :: (MonadWriter [(String, Bool)] m, MonadIO m) => a -> String -> m (Maybe a)
testSuccess res msg = tell [(msg, True )] >> return (Just res)


testFail :: (MonadWriter [(String, Bool)] m, MonadIO m) => String -> m (Maybe a)
testFail        msg = tell [(msg, False)] >> return Nothing

