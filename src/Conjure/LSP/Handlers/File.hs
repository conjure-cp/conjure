{-# LANGUAGE DataKinds #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Conjure.LSP.Handlers.File where

import Conjure.LSP.Util (getDiagnostics, getErrorsFromText, sendErrorMessage)
import Conjure.Prelude
import Control.Lens
import Data.Text (pack)
import Language.LSP.Diagnostics (partitionBySource)
import Language.LSP.Protocol.Lens (HasParams (..), HasTextDocument (textDocument), HasUri (uri))
import Language.LSP.Protocol.Types (Uri, toNormalizedUri)
import Language.LSP.Server (Handlers, LspM, getVirtualFile, notificationHandler, publishDiagnostics)
import Language.LSP.VFS
import Prettyprinter
import Language.LSP.Protocol.Message (SMethod(SMethod_TextDocumentDidClose, SMethod_TextDocumentDidChange, SMethod_TextDocumentDidOpen, SMethod_CancelRequest, SMethod_TextDocumentDidSave))

fileHandlers :: Handlers (LspM ())
fileHandlers = mconcat [fileOpenedHandler, fileChangedHandler, fileClosedHandler]

unhandled :: [Handlers (LspM ())]
unhandled =
  [ notificationHandler SMethod_CancelRequest $ \_ -> pure (),
    notificationHandler SMethod_TextDocumentDidSave $ \_ -> pure ()
  ]

fileOpenedHandler :: Handlers (LspM ())
fileOpenedHandler = notificationHandler SMethod_TextDocumentDidOpen $ \req -> do
  let td = req ^. params . textDocument
  doDiagForDocument td
  pure ()

fileChangedHandler :: Handlers (LspM ())
fileChangedHandler = notificationHandler SMethod_TextDocumentDidChange $ \req -> do
  let td = req ^. params . textDocument
  doDiagForDocument td
  pure ()

-- handle this only to suppress not implemented message
fileClosedHandler :: Handlers (LspM ())
fileClosedHandler = notificationHandler SMethod_TextDocumentDidClose $ \_ -> pure ()

doDiagForDocument :: (HasUri a Uri, Show a) => a -> LspM () ()
doDiagForDocument d = do
  let td = d ^. uri
  let doc = toNormalizedUri td
  mdoc <- getVirtualFile doc
  case mdoc of
    Just vf@(VirtualFile _ version _rope) -> do
      errs <- getErrorsFromText $ virtualFileText vf
      case errs of
        Left msg -> sendErrorMessage "An error occured:details incoming" >> sendErrorMessage msg
        Right file -> publishDiagnostics 10000 doc (Just $ fromIntegral version) $ partitionBySource $ getDiagnostics file
    _ -> sendErrorMessage $ pack . show $ "No virtual file found for: " <> stringToDoc (show d)