module Conjure.UI.IO
    ( readModelFromFile
    , readModelFromStdin
    , readModelPreambleFromFile
    , readModelInfoFromFile
    , readParamJSON
    , readParamOrSolutionFromFile
    , writeModel, writeModels
    , readModel
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.UI
import Conjure.Language
import qualified Conjure.Language.Parser as Parser
import qualified Conjure.Language.ParserC as ParserC
import Conjure.Language.Parser ( Parser )

-- aeson
import qualified Data.Aeson ( eitherDecodeStrict )

-- cereal
import qualified Data.Serialize ( decode, encode )

-- text
import qualified Data.Text as T
import qualified Data.Text.IO as T ( getContents )
import qualified Data.Text.Encoding as T ( encodeUtf8 )

-- bytestring
import qualified Data.ByteString as BS ( readFile, writeFile )
import qualified Data.ByteString.Char8 as BS ( putStrLn )


readModelFromFile ::
    MonadIO m =>
    MonadFailDoc m =>
    MonadUserError m =>
    FilePath -> m Model
readModelFromFile fp = do
    con <- liftIO $ BS.readFile fp
    case Data.Serialize.decode con of
        Right res -> return res
        Left _ -> do
            pair <- liftIO $ pairWithContents fp
            readModel Parser.parseModel (Just id) pair


readModelFromStdin ::
    MonadIO m =>
    MonadFailDoc m =>
    MonadUserError m =>
    m Model
readModelFromStdin = do
    con2 <- liftIO $ T.getContents
    let pair = ("stdin", con2)
    readModel Parser.parseModel (Just id) pair


readParamJSON ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    MonadIO m =>
    MonadUserError m =>
    MonadLog m =>
    Model -> FilePath -> m Model
readParamJSON model fp = do
    (_, contents) <- liftIO $ pairWithContents fp
    let paramJSON = contents
                    |> T.encodeUtf8                     -- convert Text to ByteString
                    |> Data.Aeson.eitherDecodeStrict
    case paramJSON of
        Left err -> userErr1 (pretty err)
        Right j -> fromSimpleJSONModel model j


readParamOrSolutionFromFile ::
    (?typeCheckerMode :: TypeCheckerMode) =>
    MonadIO m =>
    MonadFailDoc m =>
    MonadUserError m =>
    MonadLog m =>
    Model -> FilePath -> m Model
readParamOrSolutionFromFile model fp = do
    if ".json" `isSuffixOf` fp
        then readParamJSON model fp
        else do
            con <- liftIO $ BS.readFile fp
            case Data.Serialize.decode con of
                Right res -> return res
                Left _ -> do
                    pair <- liftIO $ pairWithContents fp
                    readModel ParserC.parseModel (Just id) pair


readModelPreambleFromFile ::
    MonadIO m =>
    MonadFailDoc m =>
    MonadUserError m =>
    FilePath -> m Model
readModelPreambleFromFile fp = do
    con <- liftIO $ BS.readFile fp
    case Data.Serialize.decode con of
        Right res -> return res
        Left _ -> do
            pair <- liftIO $ pairWithContents fp
            readModel Parser.parseModel (Just onlyPreamble) pair

readModelInfoFromFile ::
    MonadIO m =>
    MonadFailDoc m =>
    MonadUserError m =>
    FilePath -> m Model
readModelInfoFromFile fp = do
    con <- liftIO $ BS.readFile fp
    case Data.Serialize.decode con of
        Right res -> return res
        Left _ -> do
            pair <- liftIO $ pairWithContents fp
            readModel Parser.parseModel Nothing pair


readModel ::
    MonadFailDoc m =>
    MonadUserError m =>
    Parser Model ->
    Maybe (Text -> Text) ->
    (FilePath, Text) ->
    m Model
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
            |> Data.Aeson.eitherDecodeStrict

    if T.null (T.filter isSpace infoBlock)
        then return model
        else
            case infoJson of
                Left err -> userErr1 $ vcat
                    [ "Malformed JSON in a cached Essence Prime model."
                    , "It could be created by a different version of Conjure or modified by hand."
                    , ""
                    , pretty err
                    ]
                Right i -> return model { mInfo = i }


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


writeModel ::
    MonadIO m =>
    MonadUserError m =>
    Int ->
    OutputFormat ->
    Maybe FilePath ->
    Model ->
    m ()
writeModel  lnWidth Plain   Nothing   spec
    | lnWidth == 0                         = liftIO $    putStrLn     (show           spec)
    | otherwise                            = liftIO $    putStrLn     (render lnWidth spec)
writeModel  lnWidth Plain   (Just fp) spec
    | lnWidth == 0                         = liftIO $    writeFile fp (show           spec)
    | otherwise                            = liftIO $    writeFile fp (render lnWidth spec)
writeModel _lnWidth Binary  Nothing   spec = liftIO $ BS.putStrLn     (Data.Serialize.encode spec)
writeModel _lnWidth Binary  (Just fp) spec = liftIO $ BS.writeFile fp (Data.Serialize.encode spec)
writeModel  lnWidth ASTJSON Nothing   spec
    | lnWidth == 0                         = liftIO $    putStrLn     (show           (toJSON spec))
    | otherwise                            = liftIO $    putStrLn     (render lnWidth (toJSON spec))
writeModel  lnWidth ASTJSON (Just fp) spec
    | lnWidth == 0                         = liftIO $    writeFile fp (show           (toJSON spec))
    | otherwise                            = liftIO $    writeFile fp (render lnWidth (toJSON spec))
writeModel lnWidth fmt Nothing spec | fmt `elem` [JSON, JSONStream] = do
    spec' <- toSimpleJSON spec
    if lnWidth == 0
        then liftIO $ putStrLn (show spec')
        else liftIO $ putStrLn (render lnWidth spec')
writeModel lnWidth fmt (Just fp) spec | fmt `elem` [JSON, JSONStream] = do
    spec' <- toSimpleJSON spec
    if lnWidth == 0
        then liftIO $ writeFile fp (show spec')
        else liftIO $ writeFile fp (render lnWidth spec')
writeModel lnWidth MiniZinc Nothing spec = do
    spec' <- toMiniZinc spec
    if lnWidth == 0
        then liftIO $ putStrLn (show spec')
        else liftIO $ putStrLn (render lnWidth spec')
writeModel lnWidth MiniZinc (Just fp) spec = do
    spec' <- toMiniZinc spec
    if lnWidth == 0
        then liftIO $ writeFile fp (show spec')
        else liftIO $ writeFile fp (render lnWidth spec')
writeModel _ _ _ _ = bug "writeModels"


writeModels ::
    MonadIO m =>
    MonadUserError m =>
    Int ->
    OutputFormat ->
    FilePath ->
    String ->
    [Model] ->
    m ()
writeModels lnWidth mode base tag specs = do
    let numbers = map (padShowInt 4) [ (1 :: Int) .. ]
    let outDirname  = base ++ "-" ++ tag
    liftIO $ createDirectoryIfMissing True outDirname
    forM_ (zip numbers specs) $ \ (i, spec) -> do
        let outFilename = base ++ "-" ++ tag ++ "/" ++ i ++ ".essence"
        writeModel lnWidth mode (Just outFilename) spec
        liftIO $ putStrLn $ "[created file] " ++ outFilename

