module Test.Language.SpecParse where


import System.Directory ( doesFileExist )
import System.IO.Unsafe ( unsafeInterleaveIO )
import Test.HUnit -- ( Test(..), (~:), (~=?), test )

import Language.EssenceParsers ( pSpec )
import ParsecUtils ( parseFromFile )
import Utils ( ppShow )


allTests :: [String] -> IO Test
-- allTests files = test <$> mapM testParseSpec files
allTests files = return . test $ map (TestLabel "testParseSpec" . TestCase . testParseSpec) files


-- given a filename and its contents as a pair of strings, attempts to parse
-- the input as an Essence Spec.
testParseSpec :: String -> Assertion
testParseSpec filename = do
    sp <- parseFromFile pSpec id filename id

    let out = ppShow sp

    let expectedfile = filename ++ ".ppPrint"
    expectedfileExists <- doesFileExist expectedfile
    expected <- unsafeInterleaveIO $ readFile expectedfile

    case (expectedfileExists, expected == out) of
        (True , True ) -> return ()
        (False, _    ) -> do writeFile (filename++".generated") out
                             assertFailure $ expectedfile ++ " doesn't exist."
        (_    , False) -> assertFailure "expected /= generated"
