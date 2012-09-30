{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.ConjureRefn where

import Language.E
import Language.E.Pipeline.ReadIn ( readSpec, readRuleRefn )
import Language.E.Pipeline.RuleRefnToFunction ( ruleRefnToFunction )
import Language.E.Pipeline.ApplyRefn ( applyRefn )
import Language.E.Pipeline.RemoveUnused ( removeUnused )
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Language.E.Pipeline.NoTuples ( noTuplesSpec )


conjureRefn :: (Functor m, Monad m)
    => (FilePath, Text)
    -> [(FilePath, Text)]
    -> [(FilePath, Text)]
    -> CompE m Spec
conjureRefn spectobe rulestobe _reprstobe = do
    spec  <- readSpec spectobe
    rules <- mapM readRuleRefn rulestobe
    -- reprs <- mapM readRuleRepr reprstobe
    -- mkLog "toCore" $ stringToDoc $ show reprs
    -- return spec

    case ruleRefnToFunction (concat rules) of
        Left  es -> err ErrFatal $ vcat $ map snd es
        Right fs ->
            let pipeline =  applyRefn fs
                        >=> trySimplifySpec
                        >=> removeUnused
                        >=> makeIdempotent noTuplesSpec
                        >=> trySimplifySpec
                        >=> (return . atMostOneSuchThat) 
            in  pipeline spec

