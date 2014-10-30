module Conjure.UI.IO
    ( readModelFromFile
    , readModelPreambleFromFile
    , writeModel, writeModels
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Parser
import Conjure.Language.Pretty

-- aeson
import qualified Data.Aeson ( decode )

-- text
import qualified Data.Text as T
import qualified Data.Text.Encoding as T ( encodeUtf8 )

-- bytestring
import qualified Data.ByteString.Lazy as BS


readModelFromFile :: (MonadIO m, MonadFail m) => FilePath -> m Model
readModelFromFile fp = do
    pair <- liftIO $ pairWithContents fp
    readModel pair

readModelPreambleFromFile :: FilePath -> IO Model
readModelPreambleFromFile fp = do
    pair <- liftIO $ pairWithContents fp
    readModelPreamble pair

readModel :: MonadFail m => (FilePath, Text) -> m Model
readModel (fp, con) =
    case runLexerAndParser parseModel fp con of
        Left  e -> userErr e
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
                    |> (BS.fromChunks . return)         -- convert to Lazy ByteString
                    |> Data.Aeson.decode
            in
                if T.null (T.filter isSpace infoBlock)
                    then return x
                    else
                        case json of
                            Nothing -> fail "Malformed JSON"
                            Just i  -> return x { mInfo = i }


readModelPreamble :: MonadFail m => (FilePath, Text) -> m Model
readModelPreamble (fp,con) =
    case runLexerAndParser parseModel fp (onlyPreamble con) of
        Left  e -> userErr e
        Right x -> return x
    where
        stripComments = T.unlines . map (T.takeWhile (/= '$')) . T.lines
        discardAfter t = fst . T.breakOn t
        onlyPreamble
            = discardAfter "maximising"
            . discardAfter "maximizing"
            . discardAfter "minimising"
            . discardAfter "minimizing"
            . discardAfter "such that"
            . stripComments


writeModel :: MonadIO m => Maybe FilePath -> Model -> m ()
writeModel Nothing   spec = liftIO $ putStrLn     (renderNormal spec)
writeModel (Just fp) spec = liftIO $ writeFile fp (renderNormal spec)


writeModels :: MonadIO m => FilePath -> String -> [Model] -> m ()
writeModels base tag specs = do
    let numbers = map (padShowInt 4) [ (1 :: Int) .. ]
    forM_ (zip numbers specs) $ \ (i, spec) -> do
        let outDirname  = base ++ "-" ++ tag
        let outFilename = base ++ "-" ++ tag ++ "/" ++ i ++ ".essence"
        liftIO $ do
            createDirectoryIfMissing True outDirname
            writeModel (Just outFilename) spec
            putStrLn $ "[created file] " ++ outFilename

