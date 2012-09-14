{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Language.E.Pipeline.ToCore where

import Language.E
import Language.E.Pipeline.ReadIn
import Language.E.Pipeline.RuleRefnToFunction
import Language.E.Pipeline.ApplyTransformation
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )


toCore :: (Functor m, Monad m)
    => (FilePath, Text)
    -> [(FilePath, Text)]
    -> [(FilePath, Text)]
    -> CompE m Spec
toCore spectobe rulestobe _reprstobe = do
    spec  <- readSpec spectobe
    rules <- mapM readRuleRefn rulestobe
    -- reprs <- mapM readRuleRepr reprstobe
    -- mkLog "toCore" $ stringToDoc $ show reprs
    -- return spec

    case ruleRefnToFunction (concat rules) of
        Left  es -> err ErrFatal $ vcat $ map snd es
        Right fs ->
            let pipeline =  applyTransformation fs
                        >=> (return . atMostOneSuchThat)
            in  pipeline spec

