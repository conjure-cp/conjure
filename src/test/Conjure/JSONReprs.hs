{-# LANGUAGE RecordWildCards #-}

module Conjure.JSONReprs ( tests ) where

import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Pretty ( pretty, (<++>), renderNormal )
import Conjure.Language.Type ( TypeCheckerMode(..) )

import Conjure.UI ( OutputFormat(..) )
import Conjure.UI.IO ( readModelFromFile, writeModel )


-- base
import System.Info ( os )

-- tasty
import Test.Tasty ( TestTree, testGroup )
import Test.Tasty.HUnit ( testCaseSteps, assertFailure )

import Data.Aeson.Diff
import qualified Data.Aeson as JSON
import Data.Aeson (decode')
import qualified Data.Text
import Data.ByteString.Builder (toLazyByteString)
import Data.Text.Encoding (encodeUtf8Builder)

tests ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    IO TestTree
tests = do
    let baseDir = "tests/json_repr"
    dirs <- mapM (isTestDir baseDir) =<< getAllDirs baseDir
    let testCases = map testSingleDir (catMaybes dirs)
    return (testGroup "json_repr" testCases)


data TestDirFiles = TestDirFiles
    { name           :: String          -- a name for the test case
    , tBaseDir       :: FilePath        -- dir
    , essenceFile    :: FilePath        -- dir + filename
    }
    deriving Show


-- returns True if the argument points to a directory that is not hidden
isTestDir :: FilePath -> FilePath -> IO (Maybe TestDirFiles)
isTestDir baseDir dir = do
    essenceFiles <- filter (".essence" `isSuffixOf`) <$> getDirectoryContents dir
    case essenceFiles of
        [f] -> return $ Just TestDirFiles
            { name           = drop (length baseDir + 1) dir
            , tBaseDir       = dir
            , essenceFile    = dir </> f
            }
        _ -> return Nothing



-- the first FilePath is the base directory for the parse_print tests
-- we know at this point that the second FilePath points to a directory D,
-- which contains + an Essence file D/D.essence
testSingleDir ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    TestDirFiles -> TestTree
testSingleDir TestDirFiles{..} = testCaseSteps (map (\ ch -> if ch == '/' then '.' else ch) name) $ \ step -> do
    step "Conjuring"
    model_ <- runUserErrorT (readModelFromFile essenceFile)
    result <-case model_ of
            Left err    -> return (Left err)
            Right model -> return (Right model)
    case result of
        Left err    -> do
            writeFile (tBaseDir </> "stderr") (show err)
            removeFileIfExists (tBaseDir </> "model.json")
        Right model -> do
            writeModel 120 ASTJSON (Just (tBaseDir </> "model.json")) model
            removeFileIfExists (tBaseDir </> "stderr")

    let

        fixWindowsPaths :: String -> String
        fixWindowsPaths
            | os `elem` ["mingw32"] = fixBackslashes
            | otherwise             = id

        fixBackslashes :: String -> String
        fixBackslashes ('/'  : '\\' : xs) = "/\\" ++ fixBackslashes xs
        fixBackslashes ('\\' : '/'  : xs) = "\\/" ++ fixBackslashes xs
        fixBackslashes ('\\'        : xs) = '/'    : fixBackslashes xs
        fixBackslashes [] = []
        fixBackslashes (x:xs) = x : fixBackslashes xs

        readIfExists :: FilePath -> IO String
        readIfExists f = fromMaybe "" <$> readFileIfExists f



    step "Checking stderr"
    stderrG <- fixWindowsPaths <$> readIfExists (tBaseDir </> "stderr")
    stderrE <- readIfExists (tBaseDir </> "stderr.expected")
    --Dont care about the contents of stderr here just that they both terminate the same
    unless (null stderrE == null stderrG) $
        assertFailure $ renderNormal $ vcat [ "unexpected stderr:" <++> pretty stderrG
                                            , "was expecting:    " <++> pretty stderrE ]
    (unless $ (not $ null stderrG)) $ do
        step "Checking Generated Representation"
        stdoutG <- fixWindowsPaths <$> readIfExists (tBaseDir </> "model.json")
        stdoutE <- readIfExists (tBaseDir </> "model.expected.json")
        let diffs = do
                    jGiven <- stringToJson stdoutG
                    jReference <- stringToJson stdoutE
                    let Patch ds =  diff jGiven jReference
                    return ds
        case diffs of
            Nothing -> assertFailure $ "JSON parser error in" ++ stdoutE
            Just [] -> return ()
            Just ops -> assertFailure $ renderNormal $ vcat [ "Difference in json:" <++> vcat (map (stringToDoc. show) ops) ]



stringToJson :: String -> Maybe JSON.Value
stringToJson s = decode' $ toLazyByteString $ encodeUtf8Builder $ Data.Text.pack s
