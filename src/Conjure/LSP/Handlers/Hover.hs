module Conjure.LSP.Handlers.Hover where
import Language.LSP.Server (requestHandler, LspM, Handlers)
import Language.LSP.Types

import Conjure.Prelude
import Data.Text as T (pack)
import Conjure.LSP.Util (getRelevantRegions, withProcessedDoc, ProcessedFile (ProcessedFile), snippet)
import Conjure.Language.Validator (RegionInfo (..), RegionType (..))
import Prettyprinter

import Conjure.LSP.Documentation ( getDocsForBuiltin)
import qualified Conjure.Language.Pretty as CPr

hoverHandler :: Handlers (LspM ())
hoverHandler = requestHandler STextDocumentHover $ \ req res -> do
    let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
    let Position _l _c' = pos
    withProcessedDoc _doc $ \(ProcessedFile _ _ st) -> do
        let ranges = getRelevantRegions st pos
        texts <- mapM prettySymbol ranges
        let ms = HoverContents  $ mconcat (catMaybes texts)
        let range = Range pos pos
        let rsp = Hover ms (Just range)
        res (Right $ Just rsp)


prettySymbol :: RegionInfo -> LspM () (Maybe MarkupContent)
prettySymbol (RegionInfo _ _ dt _ _) = case dt of
    Definition nm ty -> return $ Just . snippet . pack.show $ hcat [pretty $ nm ," : ",pretty.show $ CPr.pretty ty]
    LiteralDecl{} -> return Nothing
    Ref nm k _ -> return . Just .snippet . pack.show $ hcat [pretty $ nm," : ",pretty.show $ CPr.pretty k] --pack.show $ vcat [hcat [text.unpack $ nm ,":",pretty ty]," Declared : "<> pretty (sourcePosToPosition sp)]
    Documentation {} -> liftIO $ getDocsForBuiltin dt
    _ -> return Nothing

