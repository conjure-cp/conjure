module Conjure.UI.IO
    ( readModelFromFile
    , readModelPreambleFromFile
    , writeModel, writeModels
    , readModel
    ) where

-- conjure
import Conjure.Prelude
import Conjure.UserError
import Conjure.UI
import Conjure.Language.Definition
import Conjure.Language.Parser
import Conjure.Language.Pretty

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


readModelFromFile :: (MonadIO m, MonadFail m, MonadUserError m) => FilePath -> m Model
readModelFromFile fp = do
    con <- liftIO $ BS.readFile fp
    case Data.Serialize.decode con of
        Right res -> return res
        Left _ -> do
            pair <- liftIO $ pairWithContents fp
            readModel id pair


readModelPreambleFromFile :: (MonadIO m, MonadFail m, MonadUserError m) => FilePath -> m Model
readModelPreambleFromFile fp = do
    con <- liftIO $ BS.readFile fp
    case Data.Serialize.decode con of
        Right res -> return res
        Left _ -> do
            pair <- liftIO $ pairWithContents fp
            readModel onlyPreamble pair


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
                infoJson = infoBlock
                    |> T.encodeUtf8                     -- convert Text to ByteString
                    |> Data.Aeson.decodeStrict
            in
                if T.null (T.filter isSpace infoBlock)
                    then return x
                    else
                        case infoJson of
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


writeModel :: MonadIO m => Int -> OutputFormat -> Maybe FilePath -> Model -> m ()
writeModel  lnWidth Plain  Nothing   spec = liftIO $    putStrLn     (render lnWidth spec)
writeModel  lnWidth Plain  (Just fp) spec = liftIO $    writeFile fp (render lnWidth spec)
writeModel _lnWidth Binary Nothing   spec = liftIO $ BS.putStrLn     (Data.Serialize.encode spec)
writeModel _lnWidth Binary (Just fp) spec = liftIO $ BS.writeFile fp (Data.Serialize.encode spec)
writeModel  lnWidth JSON   Nothing   spec = liftIO $    putStrLn     (render lnWidth (toJSON spec))
writeModel  lnWidth JSON   (Just fp) spec = liftIO $    writeFile fp (render lnWidth (toJSON spec))


writeModels :: MonadIO m => Int -> OutputFormat -> FilePath -> String -> [Model] -> m ()
writeModels lnWidth mode base tag specs = do
    let numbers = map (padShowInt 4) [ (1 :: Int) .. ]
    let outDirname  = base ++ "-" ++ tag
    liftIO $ createDirectoryIfMissing True outDirname
    forM_ (zip numbers specs) $ \ (i, spec) -> do
        let outFilename = base ++ "-" ++ tag ++ "/" ++ i ++ ".essence"
        writeModel lnWidth mode (Just outFilename) spec
        liftIO $ putStrLn $ "[created file] " ++ outFilename

