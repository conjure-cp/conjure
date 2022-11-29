module Conjure.LSP.Handlers.Hover where
import Language.LSP.Server (requestHandler, LspM, Handlers, sendNotification)
import Language.LSP.Types
import Control.Lens
import Conjure.Prelude
import Data.Text as T (pack, concat, unpack)
import Conjure.LSP.Util (getProcessedDoc, getRelevantRegions, withProcessedDoc, ProcessedFile (ProcessedFile), sourcePosToPosition, snippet)
import Conjure.Language.Validator (RegionInfo (..), DeclarationType (..), DiagnosticRegion (DiagnosticRegion, drSourcePos))
import Conjure.Language (Pretty(pretty))
import Text.PrettyPrint (text)

hoverHandler :: Handlers (LspM ())
hoverHandler = requestHandler STextDocumentHover $ \ req res -> do
    let RequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
    let Position _l _c' = pos
    withProcessedDoc _doc $ \(ProcessedFile _ _ st) -> do
        let ranges = getRelevantRegions st pos
        let texts = map prettySymbol ranges
        let ms = HoverContents $ MarkupContent MkMarkdown $ T.concat (catMaybes texts)
        let range = Range pos pos
        let rsp = Hover ms (Just range)
        res (Right $ Just rsp)


prettySymbol :: RegionInfo -> Maybe Text
prettySymbol (RegionInfo dr ty nm dt) = case dt of
    Definition -> Just . snippet . pack.show $ hcat [text.unpack $ nm ," : ",pretty ty]
    LiteralDecl -> Nothing
    Ref DiagnosticRegion{drSourcePos=sp} -> Just .snippet . pack.show $ hcat [text.unpack $ nm," : ",pretty ty] --pack.show $ vcat [hcat [text.unpack $ nm ,":",pretty ty]," Declared : "<> pretty (sourcePosToPosition sp)]
    Ref _ -> Nothing
