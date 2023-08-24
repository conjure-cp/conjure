module Conjure.LSP.Handlers.SemanticTokens where
import Language.LSP.Server (requestHandler, LspM, Handlers)
import qualified Language.LSP.Types as T
import Control.Lens
import Conjure.Prelude
import Conjure.LSP.Util (withProcessedDoc, ProcessedFile (ProcessedFile), sourcePosToPosition,  sendErrorMessage)
import Conjure.Language.Validator (ValidatorState (symbolCategories), TaggedToken (TaggedToken), TagType (..))
import Language.LSP.Types.Lens (HasParams(..), HasTextDocument (textDocument))
import Conjure.Language.Lexer (ETok(..), Offsets (..), trueLength)
import Language.LSP.Types (SemanticTokenTypes(..), SemanticTokenModifiers (..))
import qualified Data.Map as M

semanticTokensHandler :: Handlers (LspM ())
semanticTokensHandler = semanticTokensHandlerFull


semanticTokensHandlerFull :: Handlers (LspM ())
semanticTokensHandlerFull = requestHandler T.STextDocumentSemanticTokensFull $ \ req res -> do
    let ps = req^.params . textDocument
    withProcessedDoc ps $ \(ProcessedFile _ _ (symbolCategories->ts) _) -> do
        let toks = mapMaybe createSemanticToken (sortOn (\(TaggedToken _ (oTrueStart . offsets->e)) -> e) $ M.elems ts)
        -- sendInfoMessage . pack $  "Got semantic tokens req : " ++ show toks
        let sToks = T.makeSemanticTokens def toks
        r <-case sToks of
          Left txt -> sendErrorMessage txt >> return (Right Nothing)
          Right st -> return . Right . pure $ st
        res r


createSemanticToken :: TaggedToken -> Maybe T.SemanticTokenAbsolute
createSemanticToken (TaggedToken tt tok) = 
  (uncurry (T.SemanticTokenAbsolute ln col len)) <$> symbolType tt
  where
    T.Position ln col = sourcePosToPosition (oSourcePos . offsets $ tok)
    len = fromInteger . toInteger . trueLength  $ tok
    symbolType :: TagType -> Maybe (T.SemanticTokenTypes,[T.SemanticTokenModifiers])
    symbolType s = case s of
      TtType -> Just (SttType,[StmDefaultLibrary])
      TtNumber -> Just (SttNumber,[])
      TtBool -> Just (SttRegexp,[])
      TtDomain -> Just (SttClass,[StmAbstract])
      TtEnum -> Just (SttEnum,[StmAbstract])
      TtEnumMember -> Just (SttEnumMember,[])
      TtRecord -> Just (SttStruct,[])
      TtRecordMember -> Just (SttProperty,[StmReadonly])
      TtUserFunction -> Just (SttFunction,[])
      TtFunction -> Just (SttFunction,[StmDefaultLibrary])
      TtAttribute -> Just (SttVariable,[StmReadonly])
      TtAAC -> Just (SttInterface,[])
      TtVariable -> Just (SttVariable,[])
      TtKeyword -> Just (SttKeyword,[])
      TtQuantifier -> Just (SttMacro,[])
      TtSubKeyword -> Just (SttModifier,[])
      TtOperator -> Just (SttOperator,[])
      TtLocal -> Just (SttParameter,[])
      TtOther _ -> Nothing

