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
    -> CompE m Spec
toCore spectobe rulestobe = do
    mkLog "debug:toCore" "in toCore 1"
    spec  <- readSpec spectobe
    -- spec' <- traverseSpec Nothing foo Nothing spec
    -- return spec'
    mkLog "debug:toCore" "in toCore 2"
    rules <- mapM readRuleRefn rulestobe
    mkLog "debug:toCore" "in toCore 3"
    case ruleRefnToFunction (concat rules) of
        Left  es -> err ErrFatal $ vcat $ map snd es
        Right fs ->
            let pipeline =  applyTransformation fs
                        >=> atMostOneSuchThat
            in  pipeline spec


readSpec :: (Functor m, Monad m)
    => (FilePath, Text)
    -> CompE m Spec
readSpec (fp,con) =
    case runLexerAndParser parseSpec fp con of
        Left  e -> err ErrFatal e
        Right x -> atMostOneSuchThat x


readRuleRefn :: (Functor m, Monad m)
    => (FilePath, Text)
    -> CompE m [RuleRefn]
readRuleRefn (fp,con) =
    case runLexerAndParser (parseRuleRefn fp) fp con of
        Left  e -> err ErrFatal e
        Right x -> return x



-- intro [xMatch|  |]

foo :: Monad m => Generic BuiltIn -> m (Generic BuiltIn)
foo [xMatch| [Prim (I i)] := value.literal |] = return [xMake| value.literal := [Prim $ I $ i+1] |]
-- foo (Prim (I i)) = return $ Prim $ I $ i + 1
foo x = return x
