{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
module Conjure.LSP.Handlers.File where
import Conjure.Prelude
import Language.LSP.Server (notificationHandler, Handlers, LspM, sendNotification, publishDiagnostics, getVirtualFile)
import Language.LSP.Types (SMethod (STextDocumentDidOpen, SWindowShowMessage, STextDocumentDidChange, STextDocumentDidClose), NotificationMessage (NotificationMessage), ShowMessageParams (ShowMessageParams), MessageType (MtInfo, MtError), DidOpenTextDocumentParams (DidOpenTextDocumentParams), DidChangeTextDocumentParams (DidChangeTextDocumentParams), DidCloseTextDocumentParams (DidCloseTextDocumentParams), toNormalizedUri, Uri)
import Data.Text (pack)
import Control.Lens
import Language.LSP.VFS
import Language.LSP.Types.Lens (HasTextDocument(textDocument), HasParams (..), HasUri (uri))
import Conjure.LSP.Util (getErrorsForURI, getErrorsFromText, sendInfoMessage, sendErrorMessage, getDiagnostics)
import Language.LSP.Diagnostics (partitionBySource)

fileHandlers :: Handlers (LspM ())
fileHandlers = mconcat [fileOpenedHandler,fileChangedHandler,fileClosedHandler]

fileOpenedHandler :: Handlers (LspM ())
fileOpenedHandler = notificationHandler STextDocumentDidOpen $ \ req -> do
    let NotificationMessage _ _ (DidOpenTextDocumentParams doc) = req
    sendNotification SWindowShowMessage (ShowMessageParams MtInfo $ pack ("Opened file "++ show doc++ "\n"))
    let td = req^.params.textDocument
    doDiagForDocument td
    pure ()



fileChangedHandler :: Handlers (LspM ())
fileChangedHandler = notificationHandler STextDocumentDidChange $ \ req -> do
    let NotificationMessage _ _ (DidChangeTextDocumentParams id chg) = req
    sendInfoMessage $ pack ("Changed file "++ show chg ++ "\n")
    let td = req^.params.textDocument
    doDiagForDocument td
    pure ()

fileClosedHandler :: Handlers (LspM ())
fileClosedHandler = notificationHandler STextDocumentDidClose $ \ req -> do
    let NotificationMessage _ _ (DidCloseTextDocumentParams id) = req
    sendInfoMessage $ pack ("Closed file "++ show id ++ "\n")
    pure ()



doDiagForDocument ::( HasUri a Uri,Show a) => a -> LspM () ()
doDiagForDocument d = do
    let td = d^.uri
    let doc = toNormalizedUri td
    mdoc <- getVirtualFile doc
    case mdoc of
          Just vf@(VirtualFile _ version _rope) -> do
            errs <- getErrorsFromText $ virtualFileText vf
            case errs of
                Left msg -> sendErrorMessage msg
                Right file -> publishDiagnostics 10000 doc (Just $ fromIntegral version) $ partitionBySource $ getDiagnostics file
          _ -> sendErrorMessage $ pack. show $ "No virtual file found for: " <> stringToDoc (show d)