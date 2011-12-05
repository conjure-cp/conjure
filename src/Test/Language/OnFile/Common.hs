{-# LANGUAGE FlexibleContexts #-}

module Test.Language.OnFile.Common where

import System.Directory ( doesFileExist )
import System.IO.Unsafe ( unsafeInterleaveIO )
import Test.HUnit ( Test(..), Assertion, assertFailure, test )

import Language.Essence ( Expr, Binding, Log )
import ParsecUtils ( Parser, parseFromFile )
import Phases.ResolveTwoBars ( resolveTwoBars )
import PrintUtils ( Doc, render )
import Utils ( ppShow )
import Data.Generics.Uniplate.Direct ( Biplate )


mkAllTests :: String -> (String -> Assertion) -> [String] -> Test
mkAllTests label tester files = test $ map (TestLabel label . TestCase . tester) files


testOne
    :: (Biplate a Expr, Show a)
    => (Parser a)
    -> (a -> Maybe Doc)
    -> (a -> (Maybe String, [Log]))
    -> (a -> [Binding])
    -> String
    -> Assertion
testOne parser printer typechecker bindingsOf filename = do
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

    let inpRawOut = ppShow inp
    let rawPrintFileName = filename ++ ".raw"
    rawPrintFileExists <- doesFileExist rawPrintFileName
    rawPrintFile <- unsafeInterleaveIO $ readFile rawPrintFileName

    case (rawPrintFileExists, rawPrintFile == inpRawOut) of
        (True , True ) -> return ()
        (False, _    ) -> do writeFile (rawPrintFileName++"?") inpRawOut
                             failed $ rawPrintFileName ++ " doesn't exist.\n"
                                   ++ "Generating: '" ++ rawPrintFileName ++ "?'"
        (_    , False) -> failed "raw from file /= generated raw"

    case printer inp of
        Nothing    -> failed "Cannot render."
        Just inpOut -> do
            let inpOutRendered = render inpOut
            let expectedFileName = filename ++ ".expected"
            expectedFileExists <- doesFileExist expectedFileName
            expectedFile <- unsafeInterleaveIO $ readFile expectedFileName

            case (expectedFileExists, expectedFile == inpOutRendered) of
                (True , True ) -> return ()
                (False, _    ) -> do writeFile (expectedFileName++"?") inpOutRendered
                                     failed $ expectedFileName ++ " doesn't exist.\n"
                                                  ++ "Generating: '" ++ expectedFileName ++ "?'"
                (_    , False) -> failed "expected rendering from file /= generated rendering"
