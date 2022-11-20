module Conjure.LSP.Handlers.Hover where
import Language.LSP.Server (requestHandler, LspM, Handlers, sendNotification)
import Language.LSP.Types 
import Control.Lens
import Conjure.Prelude
import Data.Text (pack)
import Conjure.LSP.Util (getProcessedDoc, getRelevantRegions, withProcessedDoc, ProcessedFile (ProcessedFile))

hoverHandler :: Handlers (LspM ())
hoverHandler = requestHandler STextDocumentHover $ \ req res -> do

    let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
    let Position _l _c' = pos
    withProcessedDoc _doc $ \(ProcessedFile _ _ st) -> do
        let ranges = getRelevantRegions st pos
        let ms = HoverContents $ MarkupContent MkPlainText $ pack $ (show (Conjure.Prelude.length ranges)) ++": " ++ (show ranges)
        let range = Range pos pos
        let rsp = Hover ms (Just range)
        res (Right $ Just rsp)