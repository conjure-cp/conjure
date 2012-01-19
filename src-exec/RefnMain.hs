
-- | the expression refinement phase of Conjure

module Main where

import Control.Monad ( when )
import Control.Monad.Error ( runErrorT )
import Data.List ( groupBy, sortBy, isSuffixOf )
import System.Environment ( getArgs )
import System.FilePath ( dropExtension )
import System.Directory ( createDirectoryIfMissing )

import Language.Essence ( RuleRefn(..) )
import Language.EssenceParsers ( pSpec, pRuleRefn )
import Language.EssencePrinters ( prSpec )
import ParsecUtils ( parseFromFile )
import Phases.Refn ( applyRefnsToSpec )
import PrintUtils ( render )
import Utils -- ( padLeft )


main :: IO ()
main = do
    args <- getArgs
    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."
    spec  <- parseFromFile pSpec id specFilename id
    refns <- mapM (\ r -> parseFromFile (pRuleRefn r) id r id ) $ filter (".rule" `isSuffixOf`) args

    when (null refns) $ putStrLn "Warning: no *.rule file is given."

    specs <- runErrorT $ applyRefnsToSpec refns spec

    let dirName = dropExtension specFilename ++ "-refn"
    createDirectoryIfMissing True dirName

    case specs of
        Left err -> error err
        Right ss -> flip mapM_ (zip [(1::Int)..] ss) $ \ (i,s) -> do
            let outFilename = dirName ++ "/" ++ padLeft '0' 6 (show i) ++ ".essence"
            putStrLn $ "Outputting: " ++ outFilename
            writeFile outFilename $ render prSpec s

groupRefns :: [RuleRefn] -> [[RuleRefn]]
groupRefns rs = groupBy (\ r1 r2 -> refnLevel r1 ==  refnLevel r2 )
              $ sortBy  (\ r1 r2 -> refnLevel r1 `o` refnLevel r2 ) rs
    where
        o :: Ord a => Maybe a -> Maybe a -> Ordering
        o (Just i) (Just j) = compare i j
        o i j = compare j i
