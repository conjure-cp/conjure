module Conjure.UI.IO
    ( readModelFromFile
    , readModelFromFileWithMode
    , readModelPreambleFromFile
    , writeModel, writeModels
    , EssenceFileMode(..)
    ) where

-- conjure
import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Parser
import Conjure.Language.Pretty

-- base
import System.IO ( withFile, IOMode(..), hGetContents )

-- aeson
import qualified Data.Aeson ( decodeStrict )

-- cereal
import qualified Data.Serialize ( decode, encode )

-- text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T ( encodeUtf8 )

-- bytestring
import qualified Data.ByteString as BS ( readFile, writeFile )
import qualified Data.ByteString.Char8 as BS ( putStrLn )


data EssenceFileMode = BinaryEssence | PlainEssence


guessMode :: MonadIO m => FilePath -> m EssenceFileMode
guessMode fp = liftIO $ withFile fp ReadMode $ \ h -> do
    contents <- hGetContents h
    let strip (ch:xs) | ch `elem` (" \t\n" :: String) = strip xs
        strip ('$':xs) = strip $ dropWhile (/='\n') xs
        strip xs = xs
    if "language" `isPrefixOf` strip contents
        then return PlainEssence
        else return BinaryEssence


readModelFromFile :: (MonadIO m, MonadFail m, MonadUserError m) => FilePath -> m Model
readModelFromFile fp = do
    mode <- guessMode fp
    readModelFromFileWithMode mode fp


readModelFromFileWithMode :: (MonadIO m, MonadFail m, MonadUserError m) => EssenceFileMode -> FilePath -> m Model
readModelFromFileWithMode PlainEssence fp = do
    pair <- liftIO $ pairWithContents fp
    readModel id pair
readModelFromFileWithMode BinaryEssence fp = do
    con <- liftIO $ BS.readFile fp
    case Data.Serialize.decode con of
        Left err -> fail $ vcat [ "Decoding binary file failed: " <+> pretty fp
                                , pretty err
                                ]
        Right res -> return res


readModelPreambleFromFile :: (MonadIO m, MonadFail m, MonadUserError m) => FilePath -> m Model
readModelPreambleFromFile fp = do
    mode <- guessMode fp
    case mode of
        PlainEssence -> do
            pair <- liftIO $ pairWithContents fp
            readModel onlyPreamble pair
        BinaryEssence -> readModelFromFileWithMode BinaryEssence fp


readModel :: (MonadUserError m, MonadFail m) => (Text -> Text) -> (FilePath, Text) -> m Model
readModel preprocess (fp, con) =
    case runLexerAndParser parseModel fp (preprocess con) of
        Left  e -> userErr1 e
        Right x ->
            let
                infoBlock = con
                    |> T.lines
                    |> dropWhile ("$ Conjure's" /=)     -- info block heading line
                    |> drop 1                           -- drop the heading
                    |> map (T.drop 2)                   -- uncomment
                    |> T.unlines
                json = infoBlock
                    |> T.encodeUtf8                     -- convert Text to ByteString
                    |> Data.Aeson.decodeStrict
            in
                if T.null (T.filter isSpace infoBlock)
                    then return x
                    else
                        case json of
                            Nothing -> fail "Malformed JSON"
                            Just i  -> return x { mInfo = i }


onlyPreamble :: Text -> Text
onlyPreamble
    = discardAfter "maximising"
    . discardAfter "maximizing"
    . discardAfter "minimising"
    . discardAfter "minimizing"
    . discardAfter "such that"
    . stripComments
    where
        stripComments = T.unlines . map (T.takeWhile (/= '$')) . T.lines
        discardAfter t = fst . T.breakOn t


writeModel :: MonadIO m => EssenceFileMode -> Maybe FilePath -> Model -> m ()
writeModel PlainEssence   Nothing   spec = liftIO $    putStrLn     (renderNormal spec)
writeModel PlainEssence   (Just fp) spec = liftIO $    writeFile fp (renderNormal spec)
writeModel BinaryEssence Nothing   spec = liftIO $ BS.putStrLn     (Data.Serialize.encode spec)
writeModel BinaryEssence (Just fp) spec = liftIO $ BS.writeFile fp (Data.Serialize.encode spec)


writeModels :: MonadIO m => EssenceFileMode -> FilePath -> String -> [Model] -> m ()
writeModels mode base tag specs = do
    let numbers = map (padShowInt 4) [ (1 :: Int) .. ]
    forM_ (zip numbers specs) $ \ (i, spec) -> do
        let outDirname  = base ++ "-" ++ tag
        let outFilename = base ++ "-" ++ tag ++ "/" ++ i ++ ".essence"
        liftIO $ do
            createDirectoryIfMissing True outDirname
            writeModel mode (Just outFilename) spec
            putStrLn $ "[created file] " ++ outFilename

