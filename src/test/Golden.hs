module Golden ( tests ) where

-- conjure
import Conjure.Prelude

-- tasty
import Test.Tasty ( TestTree, TestName, testGroup )
import Test.Tasty.Golden.Advanced ( goldenTest )

-- shelly
import Shelly ( run )

-- text
import Data.Text as T ( Text, unpack, lines )
import Data.Text.IO as T ( readFile, writeFile )


tests :: TestTree
tests = testGroup "golden"
    [   let
            goldenFile    = "tests/golden/conjure-help.txt"
            generatedFile = goldenFile ++ ".generated"
        in
            goldenVsFile
                "help-text"
                goldenFile generatedFile
                (\ gold gen -> return $
                    let
                        goldLines  = T.lines  gold
                        goldLength = length   goldLines
                        genLines   = T.lines  gen
                        genLength  = length   genLines
                        diffs      =
                            if goldLength /= genLength
                                then
                                    [ Just [ "Different number of lines."
                                           , "    Expected: " ++ show goldLength
                                           , "    But got : " ++ show genLength
                                           ]
                                    ]
                                else
                                    [ if goldLine == genLine
                                        then Nothing
                                        else Just [ "Expected: " ++ T.unpack goldLine
                                                  , "But got : " ++ T.unpack genLine
                                                  ]
                                    -- drop 2 lines, to skip the version bit
                                    -- otherwise this test would never be able to pass!
                                    | (goldLine, genLine) <- zip (drop 2 goldLines) (drop 2 genLines)
                                    ]
                    in  case concat (catMaybes diffs) of
                            [] -> Nothing
                            ls -> Just (unlines ("Files differ.":ls)) )
                (do stdout <- sh $ run "conjure" ["--help=all"]
                    T.writeFile generatedFile stdout)
    ]


-- inlined with minimal changes from Test.Tasty.Golden.goldenVsFile
goldenVsFile
    :: TestName
    -> FilePath -> FilePath
    -> (T.Text -> T.Text -> IO (Maybe String))
    -> IO ()
    -> TestTree
goldenVsFile name ref new cmp act = goldenTest name
    (T.readFile ref)
    (liftIO act >> T.readFile new)
    (cmp)
    upd
    where upd = T.writeFile ref
