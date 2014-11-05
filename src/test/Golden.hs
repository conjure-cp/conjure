module Golden ( tests ) where

-- conjure
import Conjure.Prelude

-- tasty
import Test.Tasty ( TestTree, TestName, testGroup )
import Test.Tasty.Golden.Advanced ( goldenTest, vgReadFile )

-- shelly
import Shelly ( run )

-- text
import Data.Text as T ( unpack )

-- bytestring
import Data.ByteString.Lazy.Char8 as BS ( pack, unpack, writeFile )


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
                    -- drop 2 lines, to skip the version bit
                    -- otherwise this test would never be able to pass!
                    if drop 2 gold == drop 2 gen
                        then Nothing
                        else Just "Files differ" )
                (do stdout <- sh $ run "conjure" ["--help"]
                    BS.writeFile generatedFile (BS.pack (T.unpack stdout)))
    ]


-- inlined with minimal changes from Test.Tasty.Golden.goldenVsFile
goldenVsFile
    :: TestName
    -> FilePath -> FilePath
    -> (String -> String -> IO (Maybe String))
    -> IO ()
    -> TestTree
goldenVsFile name ref new cmp act = goldenTest name
    (vgReadFile ref)
    (liftIO act >> vgReadFile new)
    (\ a b -> cmp (BS.unpack a) (BS.unpack b) )
    upd
    where upd = BS.writeFile ref
