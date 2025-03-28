{-# LANGUAGE OverloadedStrings #-}

module Conjure.LSP.Util where

import Conjure.Language
import Conjure.Language.AST.ASTParser (parseProgram)
import Conjure.Language.AST.Reformer
import Conjure.Language.AST.Syntax (ProgramTree)
import Conjure.Language.Lexer
import Conjure.Language.Parser (PipelineError (..), lexAndParse)
import Conjure.Language.Validator (DiagnosticRegion (..), RegionInfo (..), ValidatorDiagnostic, ValidatorState (ValidatorState, regionInfo), initialState, runValidator, validateModel)
import Conjure.Language.Validator qualified as V (Diagnostic (..), ValidatorDiagnostic (..))
import Conjure.Prelude
import Conjure.UI.ErrorDisplay (displayError, displayWarning)
import Control.Lens ((^.))
import Data.Foldable (find)
import Data.Sequence qualified as Seq
import Data.Text (pack)
import Language.LSP.Protocol.Lens (HasUri (uri))
import Language.LSP.Protocol.Types as L
import Language.LSP.Server
import Language.LSP.VFS (VirtualFile, virtualFileText)
import Text.Megaparsec (SourcePos (..), mkPos, unPos)
import Language.LSP.Protocol.Message

data ProcessedFile = ProcessedFile
  { model :: Model,
    diagnostics :: [ValidatorDiagnostic],
    state :: ValidatorState,
    parseTree :: ProgramTree
  }

processFile :: Text -> Either PipelineError ProcessedFile
processFile t = do
  parsed <- lexAndParse parseProgram t
  let (m, d, s) = runValidator (validateModel parsed) (initialState parsed Nothing) -- TODO: wire up
  return $ ProcessedFile m d s parsed

getErrorsForURI :: NormalizedUri -> LspM () (Either Text ProcessedFile)
getErrorsForURI furi = do
  r <- getVirtualFile furi
  let f = maybe "" virtualFileText r
  getErrorsFromText f

getErrorsFromText :: Text -> LspM () (Either Text ProcessedFile)
getErrorsFromText t = return $ either (Left . pack . show) Right $ processFile t

getDiagnostics :: ProcessedFile -> [Diagnostic]
getDiagnostics (ProcessedFile {diagnostics = ds}) = mapMaybe valErrToDiagnostic ds

valErrToDiagnostic :: V.ValidatorDiagnostic -> Maybe Diagnostic
valErrToDiagnostic (V.ValidatorDiagnostic region message) = do
  let range = getRangeFromRegion region
  let (severity, msg) = getDiagnosticDetails message
  Just $ Diagnostic range (Just severity) Nothing Nothing Nothing msg Nothing Nothing Nothing

getRangeFromRegion :: DiagnosticRegion -> L.Range
getRangeFromRegion (DiagnosticRegion {drSourcePos = (SourcePos _ r c), drLength = l}) =
  let row = unPos r
      col = unPos c
   in Range (fixPosition row col) (fixPosition row (col + max 1 l))

getDiagnosticDetails :: V.Diagnostic -> (DiagnosticSeverity, Text)
getDiagnosticDetails x = case x of
  V.Error et -> (DiagnosticSeverity_Error, pack $ displayError et)
  V.Warning wt -> (DiagnosticSeverity_Warning, pack $ displayWarning wt)
  V.Info it -> (DiagnosticSeverity_Hint, pack $ show it)

sendInfoMessage :: Text -> LspM () ()
sendInfoMessage t = sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Info t)

sendErrorMessage :: Text -> LspM () ()
sendErrorMessage t = sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error t)

-- 0 index rows and cols as well as type coercion
fixPosition :: (Integral a) => a -> a -> Position
fixPosition r c = Position (fromIntegral r - 1) (fromIntegral c - 1)

withFile :: (HasUri a Uri, Show a) => a -> (VirtualFile -> LspM () n) -> LspM () ()
withFile fp f = do
  let td = fp ^. uri
  let doc = toNormalizedUri td
  mdoc <- getVirtualFile doc
  case mdoc of
    Just vf -> void $ f vf
    _ -> sendErrorMessage (pack . show $ "No virtual file found for: " <> pretty (show fp))

getProcessedDoc :: (HasUri a Uri, Show a) => a -> LspM () (Maybe ProcessedFile)
getProcessedDoc d = do
  let td = d ^. uri
  let doc = toNormalizedUri td
  mdoc <- getVirtualFile doc
  case mdoc of
    Just vf -> do
      case processFile $ virtualFileText vf of
        Left msg -> sendErrorMessage (pack $ show msg) >> return Nothing
        Right file -> return . pure $ file
    _ -> Nothing <$ sendErrorMessage (pack . show $ "No virtual file found for: " <> stringToDoc (show d))

withProcessedDoc :: (HasUri a Uri, Show a) => a -> (ProcessedFile -> LspM () n) -> LspM () ()
withProcessedDoc d f = do
  a <- getProcessedDoc d
  case a of
    Nothing -> return ()
    Just pf -> void $ f pf

getRelevantRegions :: ValidatorState -> Position -> [RegionInfo]
getRelevantRegions (ValidatorState {regionInfo = info}) pos = sortOn rRegion $ concatMap filteredFlatten info
  where
    p :: RegionInfo -> Bool
    p (RegionInfo {rRegion = reg}) = case reg of
      DiagnosticRegion sp sp' _ _ ->
        sourcePosToPosition sp
          <= pos
          && sourcePosToPosition sp'
          >= pos
    filteredFlatten :: RegionInfo -> [RegionInfo]
    filteredFlatten r@RegionInfo {rChildren = c} | p r = r : concatMap filteredFlatten c
    filteredFlatten _ = []

sourcePosToPosition :: SourcePos -> Position
sourcePosToPosition (SourcePos _ r c) =
  Position
    (fromInteger $ -1 + toInteger (unPos r))
    (fromInteger $ -1 + toInteger (unPos c))

positionToSourcePos :: Position -> SourcePos
positionToSourcePos (Position r c) =
  SourcePos
    ""
    (mkPos ri)
    (mkPos ci)
  where
    ri = fromIntegral r + 1
    ci = fromIntegral c + 1

regionToRange :: DiagnosticRegion -> L.Range
regionToRange (DiagnosticRegion sp ep _ _) = L.Range (sourcePosToPosition sp) (sourcePosToPosition ep)

regionToLocation :: DiagnosticRegion -> L.Location
regionToLocation reg@(DiagnosticRegion (SourcePos f _ _) _ _ _) =
  L.Location
    (filePathToUri f)
    (regionToRange reg)

snippet :: Text -> MarkupContent
snippet = mkMarkdownCodeBlock "essence"

prettyPos :: Position -> Doc
prettyPos (Position (pretty . show -> r) (pretty . show -> c)) = r <> ":" <> c

getNextTokenStart :: Position -> ProgramTree -> Position
getNextTokenStart ref tree =
  let toks = flatten tree
      tokSp = map (sourcePosToPosition . oSourcePos . offsets) (toList toks)
   in fromMaybe posInf $ find (ref <=) tokSp

posInf :: Position
posInf = Position (negate 1) (negate 1)

tokensAtPosition :: Position -> Seq.Seq ETok -> [ETok]
tokensAtPosition p s = maybeToList (find (posAfter p) s)
  where
    posAfter q t = q < sourcePosToPosition (oSourcePos $ offsets t)
