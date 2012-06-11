module Language.Core.Pipeline.ToCore where

import Language.Core
import qualified Language.Core.Middleware.RuleRefnToFunction as RuleRefnToFunction ( worker )
import qualified Language.Core.Middleware.ApplyTransformation as ApplyTransformation ( worker )
import qualified Language.Core.Middleware.AtMostOneSuchThat as AtMostOneSuchThat ( worker )


toCore :: (Functor m, Monad m)
    => (FilePath, Text)
    -> [(FilePath, Text)]
    -> CompT m Spec
toCore spectobe rulestobe = do
    spec  <- readSpec spectobe
    rules <- mapM readRuleRefn rulestobe
    case RuleRefnToFunction.worker rules of
        Left  es -> err ErrInvariant $ Nested Nothing (map snd es)
        Right fs ->
            let pipeline =  ApplyTransformation.worker fs
                        >=> AtMostOneSuchThat.worker
            in  pipeline spec


readSpec :: (Functor m, Monad m)
    => (FilePath, Text)
    -> CompT m Spec
readSpec (fp,con) = do
    s <- runP (Just fp) parseSpec con
    AtMostOneSuchThat.worker s


readRuleRefn :: (Functor m, Monad m)
    => (FilePath, Text)
    -> CompT m RuleRefn
readRuleRefn (fp,con) = runP (Just fp) (parseRuleRefn $ stringToText fp) con
