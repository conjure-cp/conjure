{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Language.E.Pipeline.ReadIn where

import Language.E
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )

import qualified Data.Set as S


readSpec :: (Functor m, Monad m)
    => (FilePath, Text)
    -> CompE m Spec
readSpec (fp,con) =
    case runLexerAndParser parseSpec fp con of
        Left  e -> err ErrFatal e
        Right x@(Spec _ statements) -> do
            let names = [ nm
                        | statement <- statements
                        , [xMatch| [Prim (S nm)] := reference |] <- universe statement
                        ]
            modifyGlobal $ \ st -> st { allNamesPreConjure = S.fromList names }
            return $! atMostOneSuchThat x


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


writeSpecs :: FilePath -> [Spec] -> IO ()
writeSpecs base specs = do
    let padShow n i = let s = show i in replicate (n - length s) '0' ++ s
    let numbers = map (padShow 4) [ (1 :: Int) .. ]
    forM_ (zip numbers specs) $ \ (i, spec) ->
        writeFile (base ++ "-" ++ i ++ ".essence") (renderPretty spec)


dropExtEssence :: String -> String
dropExtEssence = reverse . drop 8 . reverse
