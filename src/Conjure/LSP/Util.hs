{-# LANGUAGE OverloadedStrings #-}
module Conjure.LSP.Util where


import Conjure.Language.Validator (ValidatorError (..))
import Conjure.Language
import Conjure.Prelude
import Language.LSP.Server
import Language.LSP.Types
import Conjure.Language.Parser (PipelineError(..),runPipeline,parseModel)
import Language.LSP.VFS (virtualFileText)
import Conjure.Language.AST.Syntax (LToken (MissingToken, SkippedToken))
import Conjure.Language.NewLexer (ETok (..))
import Text.Megaparsec (SourcePos(..), unPos)
import Data.Text
import Conjure.Language.Expression.Op.Internal.Common (lexemeFace)
import Conjure.Language.Lexemes (lexemeText)

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

valErrToDiagnostic :: ValidatorError -> Maybe Diagnostic
valErrToDiagnostic x = case x of
        TypeError s -> Nothing
        StateError s -> Nothing
        SyntaxError s -> Nothing
        RegionError s -> Nothing
        TaggedTokenError s lt -> Nothing
        TokenError lt -> tokenErrorToD lt
        IllegalToken lt -> tokenErrorToD lt
        NotImplemented s -> Nothing


tokenErrorToD :: LToken -> Maybe Diagnostic
tokenErrorToD (MissingToken t) =Just $ makeDiagnostic t $ append "Missing: " $ lexemeText $ lexeme  t
tokenErrorToD (SkippedToken t) =Just $ makeDiagnostic t $ append "Unexpected" $ lexemeText $ lexeme t
tokenErrorToD _ = Nothing
makeDiagnostic :: ETok ->Text  -> Diagnostic
makeDiagnostic (ETok {offsets=(_,_,l,SourcePos f (unPos -> r) (unPos -> c))}) msg =  Diagnostic
    (Range 
        (Position (fromIntegral r-1) (fromIntegral c-1)) 
        (Position (fromIntegral r-1) (fromIntegral ((c-1)+(max l 1))))
    )
    (Just DsError)
    Nothing
    Nothing
    msg
    Nothing
    Nothing

sendInfoMessage :: Text -> LspM () ()
sendInfoMessage t = sendNotification SWindowShowMessage (ShowMessageParams MtInfo $ t)

sendErrorMessage :: Text -> LspM () ()
sendErrorMessage t = sendNotification SWindowShowMessage (ShowMessageParams MtError $ t)

