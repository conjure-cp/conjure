{-# LANGUAGE OverloadedStrings #-}
module Conjure.LSP.Util where


import qualified Conjure.Language.Validator as V (ValidatorDiagnostic (..), DiagnosticRegion (..), Diagnostic (..))
import Conjure.Language
import Conjure.Prelude
import Language.LSP.Server

import Conjure.Language.Parser (PipelineError(..),runPipeline,parseModel, lexAndParse)
import Language.LSP.VFS (virtualFileText, VirtualFile (VirtualFile))
import Text.Megaparsec (SourcePos(..), unPos)
import Data.Text hiding (filter)
import Language.LSP.Types as L
import Conjure.Language.Validator (DiagnosticRegion(..), ValidatorState (ValidatorState, regionInfo), runValidator, validateProgramTree, validateModel, validateModelS, ValidatorDiagnostic, RegionInfo (..))
import Conjure.UI.ErrorDisplay (displayError)
import Conjure.Language.AST.ASTParser (parseProgram)
import Language.LSP.Types.Lens (HasUri (uri))
import Language.LSP.Diagnostics (partitionBySource)
import Control.Lens ((^.))
import Text.PrettyPrint (text)
import qualified Data.Text as T

data ProcessedFile = ProcessedFile {
    model::Maybe Model,
    diagnostics::[ValidatorDiagnostic],
    state:: ValidatorState
}

processFile :: Text -> Either PipelineError ProcessedFile
processFile t = do
    parsed <- lexAndParse parseProgram t
    let (m,d,s) = runValidator (validateModelS parsed) def
    return $ ProcessedFile m d s


getErrorsForURI :: NormalizedUri -> LspM () (Either Text ProcessedFile)
getErrorsForURI uri = do
    r <- getVirtualFile uri
    let f = maybe "" virtualFileText r
    getErrorsFromText f

getErrorsFromText :: Text -> LspM () (Either Text ProcessedFile)
getErrorsFromText t = do
    return $ either (Left . pack.show) Right $ processFile t

getDiagnostics :: ProcessedFile -> [Diagnostic]
getDiagnostics (ProcessedFile _ ds _) = mapMaybe valErrToDiagnostic ds

valErrToDiagnostic :: V.ValidatorDiagnostic -> Maybe Diagnostic
valErrToDiagnostic (V.ValidatorDiagnostic region message) = do
    let range = getRangeFromRegion region
    let (severity,msg) = getDiagnosticDetails message
    Just $ Diagnostic range (Just severity) Nothing Nothing (append msg $ pack . show $ region) Nothing Nothing



getRangeFromRegion :: DiagnosticRegion -> L.Range
getRangeFromRegion GlobalRegion = Range (Position 0 0) (Position 0 0)
getRangeFromRegion (DiagnosticRegion {drSourcePos=(SourcePos _ r c),drLength=l}) =
    let row = unPos r
        col = unPos c
    in
        Range (fixPosition row col)  (fixPosition row (col+(max 1 l)))

getDiagnosticDetails :: V.Diagnostic -> (DiagnosticSeverity,Text)
getDiagnosticDetails x = case x of
  V.Error et -> (DsError,pack $displayError et)
  V.Warning wt -> (DsWarning , pack $ show wt)
  V.Info it -> (DsHint,pack $ show it)



sendInfoMessage :: Text -> LspM () ()
sendInfoMessage t = sendNotification SWindowShowMessage (ShowMessageParams MtInfo $ t)

sendErrorMessage :: Text -> LspM () ()
sendErrorMessage t = sendNotification SWindowShowMessage (ShowMessageParams MtError $ t)

-- 0 index rows and cols as well as type coercion
fixPosition :: (Integral a) => a -> a -> Position
fixPosition r c = Position (fromIntegral r-1) (fromIntegral c-1)

getProcessedDoc ::( HasUri a Uri,Show a) => a -> LspM () (Maybe ProcessedFile)
getProcessedDoc d = do
    let td = d^.uri
    let doc = toNormalizedUri td
    mdoc <- getVirtualFile doc
    case mdoc of
          Just vf@(VirtualFile _ version _rope) -> do
            case processFile $ virtualFileText vf of
                Left msg -> sendErrorMessage (pack $ show msg) >> return Nothing
                Right file -> return . pure $ file
          _ -> return Nothing <* (sendErrorMessage $ pack. show $ "No virtual file found for: " <> stringToDoc (show d))

withProcessedDoc :: ( HasUri a Uri,Show a) => a -> (ProcessedFile -> LspM () n) -> LspM () ()
withProcessedDoc d f = do
    a <- getProcessedDoc d
    case a of
      Nothing -> return ()
      Just pf -> void $ f pf


getRelevantRegions :: ValidatorState -> Position -> [RegionInfo]
getRelevantRegions (ValidatorState {regionInfo=info}) pos = sortOn (drLength . rRegion) $ filter p info
    where
        p::RegionInfo -> Bool
        p (RegionInfo {rRegion=reg}) = case reg of
          DiagnosticRegion sp sp' _ _ ->  (sourcePosToPosition sp) <= pos
                                            &&
                                          (sourcePosToPosition sp') >= pos
          GlobalRegion -> True


sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition (SourcePos _ r c) = Position
    (fromInteger $ -1 + toInteger (unPos r))
    (fromInteger $ -1 + toInteger (unPos c))

regionToRange :: DiagnosticRegion -> L.Range
regionToRange (DiagnosticRegion sp ep _ _) = L.Range (sourcePosToPosition sp) (sourcePosToPosition ep)
regionToRange GlobalRegion = error "Global region in symbol info"


snippet :: Text -> MarkupContent
snippet = markedUpContent "essence"

instance Pretty Position where
    pretty (Position (text.show->r) (text.show->c)) =  r <> ":" <> c

