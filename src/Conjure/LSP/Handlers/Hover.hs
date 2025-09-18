module Conjure.LSP.Handlers.Hover where

import Conjure.LSP.Documentation (getDocsForBuiltin)
import Conjure.LSP.Util (ProcessedFile (ProcessedFile), getRelevantRegions, snippet, withProcessedDoc)
import Conjure.Language.Pretty qualified as CPr
import Conjure.Language.Validator (RegionInfo (..), RegionType (..))
import Conjure.Prelude
import Data.Text as T (pack)
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types hiding (Definition)
import Language.LSP.Server (Handlers, LspM, requestHandler)
import Prettyprinter

hoverHandler :: Handlers (LspM ())
hoverHandler = requestHandler SMethod_TextDocumentHover $ \req res -> do
  let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
  let Position _l _c' = pos
  withProcessedDoc _doc $ \(ProcessedFile _ _ st _) -> do
    let ranges = getRelevantRegions st pos
    texts <- mapM prettySymbol ranges
    let ms = InL $ mconcat (catMaybes texts)
    let range = Range pos pos
    let rsp = Hover ms (Just range)
    res (Right $ InL rsp)

prettySymbol :: RegionInfo -> LspM () (Maybe MarkupContent)
prettySymbol (RegionInfo _ _ dt _ _) = case dt of
  Definition nm ty -> return $ Just . snippet . pack . show $ hcat [pretty nm, " : ", pretty . show $ CPr.pretty ty]
  LiteralDecl {} -> return Nothing
  Ref nm k _ -> return . Just . snippet . pack . show $ hcat [pretty nm, " : ", pretty . show $ CPr.pretty k]
  Documentation {} -> liftIO $ getDocsForBuiltin dt
  _ -> return Nothing
