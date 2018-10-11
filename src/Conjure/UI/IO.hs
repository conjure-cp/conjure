module Conjure.UI.IO
    ( readModelFromFile
    , readModelFromStdin
    , readModelPreambleFromFile
    , readModelInfoFromFile
    , readParamOrSolutionFromFile
    , writeModel, writeModels
    , readModel
    ) where

-- conjure
import Conjure.Prelude
import Conjure.UserError
import Conjure.UI
import Conjure.Language.Definition
import qualified Conjure.Language.Parser as Parser
import qualified Conjure.Language.ParserC as ParserC
import Conjure.Language.Pretty
import Conjure.Language.Parser ( Parser)

-- aeson
import qualified Data.Aeson ( decodeStrict )

-- cereal
import qualified Data.Serialize ( decode, encode )

-- text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T ( encodeUtf8 )

-- bytestring
import qualified Data.ByteString as BS ( readFile, writeFile, getContents )
import qualified Data.ByteString.Char8 as BS ( putStrLn )


readModelFromFile :: (MonadIO m, MonadFail m, MonadUserError m) => FilePath -> m Model
readModelFromFile fp = do
    con <- liftIO $ BS.readFile fp
    case Data.Serialize.decode con of
        Right res -> return res
        Left _ -> do
            pair <- liftIO $ pairWithContents fp
            readModel Parser.parseModel (Just id) pair


readModelFromStdin :: (MonadIO m, MonadFail m, MonadUserError m) => m Model
readModelFromStdin = do
    con <- liftIO $ BS.getContents
    case Data.Serialize.decode con of
        Right res -> return res
        Left _ -> do
            pair <- liftIO $ pairWithContents "stdin"
            readModel Parser.parseModel (Just id) pair


readParamOrSolutionFromFile :: (MonadIO m, MonadFail m, MonadUserError m) => FilePath -> m Model
readParamOrSolutionFromFile fp = do
    con <- liftIO $ BS.readFile fp
    case Data.Serialize.decode con of
        Right res -> return res
        Left _ -> do
            pair <- liftIO $ pairWithContents fp
            readModel ParserC.parseModel (Just id) pair


readModelPreambleFromFile :: (MonadIO m, MonadFail m, MonadUserError m) => FilePath -> m Model
readModelPreambleFromFile fp = do
    con <- liftIO $ BS.readFile fp
    case Data.Serialize.decode con of
        Right res -> return res
        Left _ -> do
            pair <- liftIO $ pairWithContents fp
            readModel Parser.parseModel (Just onlyPreamble) pair

readModelInfoFromFile :: (MonadIO m, MonadFail m, MonadUserError m) => FilePath -> m Model
readModelInfoFromFile fp = do
    con <- liftIO $ BS.readFile fp
    case Data.Serialize.decode con of
        Right res -> return res
        Left _ -> do
            pair <- liftIO $ pairWithContents fp
            readModel Parser.parseModel Nothing pair


readModel
    :: (MonadUserError m, MonadFail m)
    => Parser Model
    -> Maybe (Text -> Text)
    -> (FilePath, Text)
    -> m Model
readModel modelParser preprocess (fp, con) = do

    model <- case preprocess of
        Nothing -> return def
        Just prep ->
            case Parser.runLexerAndParser modelParser fp (prep con) of
                Left  e -> userErr1 e
                Right x -> return x

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

    if T.null (T.filter isSpace infoBlock)
        then return model
        else
            case infoJson of
                Nothing -> fail "Malformed JSON"
                Just i  -> return model { mInfo = i }


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
writeModel  lnWidth Plain  Nothing   spec
    | lnWidth == 0                        = liftIO $    putStrLn     (show           spec)
    | otherwise                           = liftIO $    putStrLn     (render lnWidth spec)
writeModel  lnWidth Plain  (Just fp) spec
    | lnWidth == 0                        = liftIO $    writeFile fp (show           spec)
    | otherwise                           = liftIO $    writeFile fp (render lnWidth spec)
writeModel _lnWidth Binary Nothing   spec = liftIO $ BS.putStrLn     (Data.Serialize.encode spec)
writeModel _lnWidth Binary (Just fp) spec = liftIO $ BS.writeFile fp (Data.Serialize.encode spec)
writeModel  lnWidth JSON   Nothing   spec
    | lnWidth == 0                        = liftIO $    putStrLn     (show           (toJSON spec))
    | otherwise                           = liftIO $    putStrLn     (render lnWidth (toJSON spec))
writeModel  lnWidth JSON   (Just fp) spec
    | lnWidth == 0                        = liftIO $    writeFile fp (show           (toJSON spec))
    | otherwise                           = liftIO $    writeFile fp (render lnWidth (toJSON spec))


writeModels :: MonadIO m => Int -> OutputFormat -> FilePath -> String -> [Model] -> m ()
writeModels lnWidth mode base tag specs = do
    let numbers = map (padShowInt 4) [ (1 :: Int) .. ]
    let outDirname  = base ++ "-" ++ tag
    liftIO $ createDirectoryIfMissing True outDirname
    forM_ (zip numbers specs) $ \ (i, spec) -> do
        let outFilename = base ++ "-" ++ tag ++ "/" ++ i ++ ".essence"
        writeModel lnWidth mode (Just outFilename) spec
        liftIO $ putStrLn $ "[created file] " ++ outFilename

