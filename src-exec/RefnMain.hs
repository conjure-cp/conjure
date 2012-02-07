
-- | the expression refinement phase of Conjure

module Main where

import Control.Monad ( forM_, when )
import Data.Function ( on )
import Data.List ( groupBy, sortBy, isSuffixOf )
import System.Directory ( createDirectoryIfMissing )
import System.Environment ( getArgs )
import System.FilePath ( dropExtension )

import Language.Essence ( RuleRefn(..) )
import Language.EssenceParsers ( pSpec, pRuleRefn )
import Language.EssencePrinters ( prSpec )
import ParsecUtils ( parseFromFile )
import Phases.QuanRename ( quanRenameIO )
import Phases.Refn ( callRefn )
import PrintUtils ( render )
import Utils ( padLeft )


main :: IO ()
main = do
    args <- getArgs
    specFilename <- case filter (".essence" `isSuffixOf`) args of
                        [t] -> return t
                        _   -> error "Only 1 *.essence file."
    spec  <- quanRenameIO =<< parseFromFile pSpec langlinePre specFilename id
    refns <- mapM (\ r -> parseFromFile (pRuleRefn r) id r id ) $ filter (".rule" `isSuffixOf`) args

    when (null refns) $ putStrLn "Warning: no *.rule file is given."

    specs <- callRefn refns spec

    let dirName = dropExtension specFilename ++ "-refn"
    createDirectoryIfMissing True dirName

    forM_ (zip [(1::Int)..] specs) $ \ (i,s) -> do
        let outFilename = dirName ++ "/" ++ padLeft '0' 6 (show i) ++ ".essence"
        putStrLn outFilename
        writeFile outFilename $ render prSpec s

groupRefns :: [RuleRefn] -> [[RuleRefn]]
groupRefns rs = groupBy ((==) `on` refnLevel)
              $ sortBy  (o    `on` refnLevel) rs
    where
        o :: Ord a => Maybe a -> Maybe a -> Ordering
        o (Just i) (Just j) = compare i j
        o i j = compare j i

langlinePre :: String -> String
langlinePre s = unlines $ "language Essence 2.0" : drop 1 (lines s)
