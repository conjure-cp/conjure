module Language.E.Pipeline.ConjureRepr where

import Language.E
import Language.E.Pipeline.ApplyRepr ( applyRepr )
import Language.E.Pipeline.AtMostOneSuchThat


conjureRepr :: (Monad m, Functor m)
    => (FilePath, Text)
    -> [(FilePath, Text)]
    -> CompE m Spec
conjureRepr spectobe rulestobe = do
    spec  <- readSpec spectobe
    rules <- mapM readRuleRepr rulestobe
    applyRepr spec rules


readSpec :: (Functor m, Monad m)
    => (FilePath, Text)
    -> CompE m Spec
readSpec (fp,con) =
    case runLexerAndParser parseSpec fp con of
        Left  e -> err ErrFatal e
        Right x -> return $ atMostOneSuchThat x


readRuleRefn :: (Functor m, Monad m)
    => (FilePath, Text)
    -> CompE m [RuleRefn]
readRuleRefn (fp,con) =
    case runLexerAndParser (parseRuleRefn fp) fp con of
        Left  e -> err ErrFatal e
        Right x -> return x

readRuleRepr :: (Functor m, Monad m)
    => (FilePath, Text)
    -> CompE m RuleRepr
readRuleRepr (fp,con) =
    case runLexerAndParser (parseRuleRepr fp) fp con of
        Left  e -> err ErrFatal e
        Right x -> return x
