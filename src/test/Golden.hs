module Golden ( tests ) where

-- conjure
import Conjure.Prelude

-- tasty
import Test.Tasty ( TestTree, TestName, testGroup )
import Test.Tasty.Golden.Advanced ( goldenTest )

-- shelly
import Shelly ( run )

-- text
import Data.Text as T ( Text, lines )
import Data.Text.IO as T ( readFile, writeFile )

-- Diff
import Data.Algorithm.Diff ( Diff(..), getGroupedDiff )
import Data.Algorithm.DiffOutput ( ppDiff )


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
                        fmapDiff f (First x) = First (f x)
                        fmapDiff f (Second x) = Second (f x)
                        fmapDiff f (Both x y) = Both (f x) (f y)

                        isBoth Both{} = True
                        isBoth _ = False

                        -- the second and third lines in the help output report
                        -- the release version and the repository version
                        dropVersionLines (x:_:_:xs) = x:xs
                        dropVersionLines xs = xs

                        -- drop the version lines, to skip that bit.
                        -- otherwise this test would never be able to pass!
                        goldLines = dropVersionLines (T.lines gold)
                        genLines  = dropVersionLines (T.lines gen)

                        diffs = filter (not . isBoth) $ getGroupedDiff goldLines genLines
                        diffsString = fmap (fmapDiff (fmap textToString)) diffs
                    in
                        if null diffs
                            then Nothing
                            else Just (unlines ["files differ.", ppDiff diffsString]) )
                (do stdout <- sh $ run "conjure" ["--help=all,120"]
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
    cmp
    upd
    where upd = T.writeFile ref
