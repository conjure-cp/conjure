module Conjure.LSP.Handlers.Format (formatHandler) where

import Conjure.LSP.Util (sendErrorMessage, withFile)
import Conjure.Language.Parser (prettyPrintWithChecks)
import Conjure.Prelude
import Control.Lens
import Data.Text qualified as T
import Language.LSP.Protocol.Lens (params, textDocument)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server (Handlers, LspM, requestHandler)
import Language.LSP.VFS (virtualFileText)
import Prettyprinter qualified as Pr
import Prettyprinter.Render.Text (renderStrict)

formatHandler :: Handlers (LspM ())
formatHandler = requestHandler SMethod_TextDocumentFormatting $ \req res -> do
  let ps = req ^. params . textDocument
  withFile ps $ \vf -> do
    let txt = virtualFileText vf
    case prettyPrintWithChecks txt of
      Left err -> do
        sendErrorMessage $ T.pack $ show err
        pure ()
      Right d -> do
        let rendered = renderStrict $ Pr.layoutSmart (Pr.LayoutOptions $ Pr.AvailablePerLine 80 0.8) d
        res $ Right $ InL $ fullEdit (fromInteger $ toInteger $ Conjure.Prelude.length $ T.lines txt) rendered

fullEdit :: UInt -> Text -> [TextEdit]
fullEdit t d =
  [ TextEdit (Range (Position 0 0) (Position t 0)) (d)
  ]
