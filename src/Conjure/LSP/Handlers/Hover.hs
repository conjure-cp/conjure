module Conjure.LSP.Handlers.Hover where
import Language.LSP.Server (requestHandler, LspM, Handlers, sendNotification)
import Language.LSP.Types 
import Control.Lens
import Conjure.Prelude
import Data.Text (pack)
hoverHandler :: Handlers (LspM ())
hoverHandler = requestHandler STextDocumentHover $ \ req res -> do
    let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
        Position _l _c' = pos
        rsp = Hover ms (Just range)
        ms = HoverContents $ markedUpContent "lsp-demo-simple-server" "Hello world"
        range = Range pos pos
    res (Right $ Just rsp)