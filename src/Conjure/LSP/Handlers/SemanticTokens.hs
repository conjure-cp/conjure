module Conjure.LSP.Handlers.SemanticTokens where
import Language.LSP.Server (requestHandler, LspM, Handlers, sendNotification, MonadLsp (getLspEnv))
import qualified Language.LSP.Types as T
import Control.Lens
import Conjure.Prelude
import Data.Text (pack,unpack,append)
import Conjure.LSP.Util (getProcessedDoc, getRelevantRegions, withProcessedDoc, ProcessedFile (ProcessedFile), regionToRange, sourcePosToPosition, sendInfoMessage, sendErrorMessage)
import Conjure.Language.Validator (RegionInfo (..), ValidatorState (regionInfo, symbolCategories), DeclarationType (..), TaggedToken (TaggedToken), TagType (..))
import Language.LSP.Types.Lens (HasParams(..), HasTextDocument (textDocument))
import Conjure.Language (Type(..))
import Conjure.Language.Type (IntTag(..))
import Conjure.Language.Pretty (Pretty(..))
import Conjure.Language.NewLexer (ETok(..), Offsets (..), trueLength)
import Language.LSP.Types (SemanticTokenTypes(..))
import qualified Data.Map as M

semanticTokensHandler :: Handlers (LspM ())
semanticTokensHandler = semanticTokensHandlerFull


semanticTokensHandlerFull :: Handlers (LspM ())
semanticTokensHandlerFull = requestHandler T.STextDocumentSemanticTokensFull $ \ req res -> do
    let ps = req^.params . textDocument
    let u =  (case ps of { (T.TextDocumentIdentifier uri) -> uri }) :: T.Uri
    withProcessedDoc ps $ \(ProcessedFile _ _ (symbolCategories->ts)) -> do
        let toks = mapMaybe createSemanticToken (sortOn (\(TaggedToken _ (oTrueStart . offsets->e)) -> e) $ M.elems ts)
        sendInfoMessage . pack $  "Got semantic tokens req : " ++ show (toks)
        let sToks = T.makeSemanticTokens def toks
        r <-case sToks of
          Left txt -> sendErrorMessage txt >> return (Right Nothing)
          Right st -> return . Right . pure $ st
        res r


createSemanticToken :: TaggedToken -> Maybe T.SemanticTokenAbsolute
createSemanticToken (TaggedToken tt tok) = T.SemanticTokenAbsolute
  (ln)
  (col)
  len
  <$>
  symbolType tt
  <*>
  pure []
  where
    T.Position ln col = sourcePosToPosition (oSourcePos . offsets $ tok)
    len = fromInteger . toInteger . trueLength  $ tok
    symbolType :: TagType -> Maybe T.SemanticTokenTypes
    symbolType s = case s of
      TtType -> Just SttType
      TtClass -> Just SttClass
      TtEnum -> Just SttEnum
      TtStruct -> Just SttStruct
      TtParameter -> Just SttParameter
      TtVariable -> Just SttVariable
      TtProperty -> Just SttProperty
      TtEnumMember -> Just SttEnumMember
      TtEvent -> Just SttEvent
      TtFunction -> Just SttFunction
      TtMethod -> Just SttMethod
      TtMacro -> Just SttMacro
      TtKeyword -> Just SttKeyword
      TtModifier -> Just SttModifier
      TtComment -> Just SttComment
      TtString -> Just SttString
      TtNumber -> Just SttNumber
      TtRegexp -> Just SttRegexp
      TtOperator -> Just SttOperator
      TtOther _ -> Nothing
