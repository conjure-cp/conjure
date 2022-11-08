{-# LANGUAGE OverloadedStrings #-}
module Conjure.LSP.Util where


import qualified Conjure.Language.Validator as V (ValidatorDiagnostic (..), DiagnosticRegion (..), Diagnostic (..)) 
import Conjure.Language
import Conjure.Prelude
import Language.LSP.Server

import Conjure.Language.Parser (PipelineError(..),runPipeline,parseModel)
import Language.LSP.VFS (virtualFileText)
import Text.Megaparsec (SourcePos(..), unPos)
import Data.Text
import Language.LSP.Types as L
import Conjure.Language.Validator (DiagnosticRegion(..))
import Conjure.UI.ErrorDisplay (displayError)

getErrorsForURI :: NormalizedUri -> LspM () (Either Text [Diagnostic])
getErrorsForURI uri = do
    r <- getVirtualFile uri
    let f = maybe "" virtualFileText r
    getErrorsFromText f

getErrorsFromText :: Text -> LspM () (Either Text [Diagnostic])
getErrorsFromText t = do
    let nots = case runPipeline parseModel t of
            Left pe -> case pe of
                ValidatorError ves -> Right $ mapMaybe valErrToDiagnostic ves
                a -> Left . pack $ show a
            Right mo -> Right []
    return nots

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