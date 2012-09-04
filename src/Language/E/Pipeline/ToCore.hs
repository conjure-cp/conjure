{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Language.E.Pipeline.ToCore where

import Language.E
import Language.E.Pipeline.RuleRefnToFunction
import Language.E.Pipeline.ApplyTransformation
import Language.E.Pipeline.AtMostOneSuchThat


toCore :: (Functor m, Monad m)
    => (FilePath, Text)
    -> [(FilePath, Text)]
    -> [(FilePath, Text)]
    -> CompE m Spec
toCore spectobe rulestobe reprstobe = do
    spec  <- readSpec spectobe
    rules <- mapM readRuleRefn rulestobe
    reprs <- mapM readRuleRepr reprstobe

    mkLog "toCore" $ stringToDoc $ show reprs

    -- case ruleRefnToFunction (concat rules) of
    --     Left  es -> err ErrFatal $ vcat $ map snd es
    --     Right fs ->
    --         let pipeline =  applyTransformation fs
    --                     >=> (return . atMostOneSuchThat)
    --         in  pipeline spec

    return spec


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


-- intro [xMatch|  |]

foo :: Monad m => Generic BuiltIn -> m (Generic BuiltIn)
foo [xMatch| [Prim (I i)] := value.literal |] = return [xMake| value.literal := [Prim $ I $ i+1] |]
-- foo (Prim (I i)) = return $ Prim $ I $ i + 1
foo x = return x
