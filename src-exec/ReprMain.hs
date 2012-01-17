
-- | the representation selection phase of Conjure

module Main where

import Control.Monad ( when )
import Control.Monad.Error ( runErrorT )
import Data.List ( isSuffixOf )
import System.Environment ( getArgs )

import Language.EssenceParsers ( pSpec, pRuleRepr )
import Language.EssencePrinters ( prSpec )
import ParsecUtils ( parseFromFile )
import Phases.Repr
import PrintUtils ( render )


main :: IO ()
main = do
    args <- getArgs
    spec <- case filter (".essence" `isSuffixOf`) args of
                [t] -> parseFromFile pSpec id t id
                _   -> error "Only 1 *.essence file."
    reprs <- mapM (\ r -> parseFromFile pRuleRepr id r id ) $ filter (".repr" `isSuffixOf`) args

    when (null reprs) $ putStrLn "Warning: no *.repr file is given."

    specs <- runErrorT $ applyToSpec reprs spec

    case specs of
        Left err -> error err
        Right ss -> mapM_ (putStrLn . render prSpec) ss

    putStrLn "Done."
