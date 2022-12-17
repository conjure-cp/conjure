module Conjure.LSP.Handlers.Format (formatHandler) where
import Language.LSP.Server (requestHandler, LspM, Handlers)
import Language.LSP.Types 
import Control.Lens
import Conjure.Prelude
import Conjure.LSP.Util (withFile, sendInfoMessage, sendErrorMessage)
import Conjure.Language.Parser (prettyPrintWithChecks)
import qualified Language.LSP.Types as Ty 
import qualified Prettyprinter as Pr
import Prettyprinter.Render.Text (renderStrict)
import Language.LSP.Types.Lens (params, textDocument)
import Language.LSP.VFS (virtualFileText)
import qualified Data.Text as T


formatHandler :: Handlers (LspM ())
formatHandler = requestHandler STextDocumentFormatting $ \ req res -> do
    let ps = req^.params.textDocument
    withFile ps $ \vf -> do
        let txt = virtualFileText vf
        case prettyPrintWithChecks txt of
                        Left err -> do
                            sendErrorMessage $ T.pack $ show err
                            pure ()
                        Right d-> do 
                            let  rendered = renderStrict $ Pr.layoutSmart (Pr.LayoutOptions $ Pr.AvailablePerLine 80 0.8) d
                            res $ Right $ fullEdit (fromInteger $ toInteger $ Conjure.Prelude.length $ T.lines txt) rendered

    

fullEdit :: UInt -> Text -> List TextEdit
fullEdit t d = Ty.List [
    TextEdit (Range (Position 0 0) (Position t 0)) (d)
    ]



