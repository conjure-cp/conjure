module Test.Language.SpecParse where


import System.Directory ( doesFileExist )
import System.IO.Unsafe ( unsafeInterleaveIO )
import Test.HUnit ( Test(..), Assertion, assertFailure, test )

import Language.EssenceParsers ( pSpec )
import Language.EssencePrinters ( prSpec )
import ParsecUtils ( parseFromFile )
import PrintUtils ( render )
import Utils ( ppShow )


allTests :: [String] -> IO Test
-- allTests files = test <$> mapM testParseSpec files
allTests files = return . test $ map (TestLabel "testParseSpec" . TestCase . testParseSpec) files


-- given a filename and its contents as a pair of strings, attempts to parse
-- the input as an Essence Spec.
testParseSpec :: String -> Assertion
testParseSpec filename = do

    let failed msg = assertFailure $ filename ++ ": " ++ msg

    sp <- parseFromFile pSpec id filename id


    let spRawOut = ppShow sp
    let rawPrintFileName = filename ++ ".raw"
    rawPrintFileExists <- doesFileExist rawPrintFileName
    rawPrintFile <- unsafeInterleaveIO $ readFile rawPrintFileName

    case (rawPrintFileExists, rawPrintFile == spRawOut) of
        (True , True ) -> return ()
        (False, _    ) -> do writeFile (rawPrintFileName++"?") spRawOut
                             failed $ rawPrintFileName ++ " doesn't exist.\n"
                                   ++ "Generating: '" ++ rawPrintFileName ++ "?'"
        (_    , False) -> failed "raw from file /= generated raw"


    case prSpec sp of
        Nothing    -> failed "Cannot render spec."
        Just spOut -> do
            let spOutRendered = render spOut
            let expectedFileName = filename ++ ".expected"
            expectedFileExists <- doesFileExist expectedFileName
            expectedFile <- unsafeInterleaveIO $ readFile expectedFileName

            case (expectedFileExists, expectedFile == spOutRendered) of
                (True , True ) -> return ()
                (False, _    ) -> do writeFile (expectedFileName++"?") spOutRendered
                                     failed $ expectedFileName ++ " doesn't exist.\n"
                                                  ++ "Generating: '" ++ expectedFileName ++ "?'"
                (_    , False) -> failed "expected rendering from file /= generated rendering"
