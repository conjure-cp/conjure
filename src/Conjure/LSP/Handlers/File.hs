{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Conjure.LSP.Handlers.File where
import Conjure.Prelude
import Language.LSP.Server (notificationHandler, Handlers, LspM,  publishDiagnostics, getVirtualFile)
import Language.LSP.Types (SMethod (..), toNormalizedUri, Uri)
import Data.Text (pack)
import Control.Lens
import Language.LSP.VFS
import Language.LSP.Types.Lens (HasTextDocument(textDocument), HasParams (..), HasUri (uri))
import Conjure.LSP.Util (getErrorsFromText,sendErrorMessage, getDiagnostics)
import Language.LSP.Diagnostics (partitionBySource)
import Prettyprinter
fileHandlers :: Handlers (LspM ())
fileHandlers = mconcat [fileOpenedHandler,fileChangedHandler,fileClosedHandler]

unhandled :: [Handlers (LspM ())]
unhandled = [ notificationHandler SCancelRequest $ \ _ -> pure ()
            , notificationHandler STextDocumentDidSave $ \ _ -> pure ()
            ]

fileOpenedHandler :: Handlers (LspM ())
fileOpenedHandler = notificationHandler STextDocumentDidOpen $ \ req -> do
    let td = req^.params.textDocument
    doDiagForDocument td
    pure ()



fileChangedHandler :: Handlers (LspM ())
fileChangedHandler = notificationHandler STextDocumentDidChange $ \ req -> do
    let td = req^.params.textDocument
    doDiagForDocument td
    pure ()
 --handle this only to suppress not implemented message
fileClosedHandler :: Handlers (LspM ())
fileClosedHandler = notificationHandler STextDocumentDidClose $ \ _ ->  pure ()



doDiagForDocument ::( HasUri a Uri,Show a) => a -> LspM () ()
doDiagForDocument d = do
    let td = d^.uri
    let doc = toNormalizedUri td
    mdoc <- getVirtualFile doc
    case mdoc of
          Just vf@(VirtualFile _ version _rope) -> do
            errs <- getErrorsFromText $ virtualFileText vf
            case errs of
                Left msg -> sendErrorMessage "An error occured:details incoming" >>sendErrorMessage msg
                Right file -> publishDiagnostics 10000 doc (Just $ fromIntegral version) $ partitionBySource $ getDiagnostics file
          _ -> sendErrorMessage $ pack. show $ "No virtual file found for: " <> stringToDoc (show d)