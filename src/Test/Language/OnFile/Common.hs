{-# LANGUAGE FlexibleContexts #-}

module Test.Language.OnFile.Common where

import System.Directory ( doesFileExist )
import System.IO.Unsafe ( unsafeInterleaveIO )
import Test.HUnit ( Test(..), Assertion, assertFailure, test )

import Language.Essence ( Expr, Binding, Log )
import ParsecUtils ( Parser, parseFromFile )
import Phases.ResolveTwoBars ( resolveTwoBars )
import PrintUtils ( Doc, render )
import Data.Generics.Uniplate.Direct ( Biplate )


mkAllTests :: String -> (String -> Assertion) -> [String] -> Test
mkAllTests label tester files = test $ map (TestLabel label . TestCase . tester) files


testOne
    :: (Biplate a Expr, Show a)
    => Parser a
    -> (a -> Maybe Doc)
    -> (a -> String)
    -> (a -> (Maybe String, [Log]))
    -> (a -> [Binding])
    -> String
    -> Assertion
testOne parser printer rawShow typechecker bindingsOf filename = do
    let failed msg = assertFailure $ filename ++ ": " ++ msg

    inp' <- parseFromFile parser id filename id

    inp <- case fst (resolveTwoBars bindingsOf inp') of
        Left err -> do
            failed $ "ambigious two-bar operator: " ++ err
            return undefined
        Right i  -> return i

    case fst (typechecker inp) of
        Nothing  -> return ()
        Just err -> failed $ "type-error: " ++ err

    let inpRawOut = rawShow inp
    let rawPrintFileName = filename ++ ".raw"
    rawPrintFileExists <- doesFileExist rawPrintFileName
    rawPrintFile <- unsafeInterleaveIO $ readFile rawPrintFileName

    case (rawPrintFileExists, rawPrintFile == inpRawOut) of
        (True , True ) -> return ()
        (False, _    ) -> do writeFile (rawPrintFileName++"?") inpRawOut
                             failed $ rawPrintFileName ++ " doesn't exist.\n"
                                   ++ "Generating: '" ++ rawPrintFileName ++ "?'"
        (_    , False) -> do writeFile (rawPrintFileName++"?") inpRawOut
                             failed $ "raw from file /= generated raw.\n"
                                   ++ "Generating: '" ++ rawPrintFileName ++ "?'"

    let inpOutRendered = render printer inp
    let expectedFileName = filename ++ ".expected"
    expectedFileExists <- doesFileExist expectedFileName
    expectedFile <- unsafeInterleaveIO $ readFile expectedFileName

    case (expectedFileExists, expectedFile == inpOutRendered) of
        (True , True ) -> return ()
        (False, _    ) -> do writeFile (expectedFileName++"?") inpOutRendered
                             failed $ expectedFileName ++ " doesn't exist.\n"
                                          ++ "Generating: '" ++ expectedFileName ++ "?'"
        (_    , False) -> do writeFile (expectedFileName++"?") inpOutRendered
                             failed $ "expected rendering from file /= generated rendering.\n"
                                          ++ "Generating: '" ++ expectedFileName ++ "?'"
