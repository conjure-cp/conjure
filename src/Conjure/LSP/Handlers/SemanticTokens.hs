module Conjure.LSP.Handlers.SemanticTokens where

import Conjure.LSP.Util (ProcessedFile (ProcessedFile), sendErrorMessage, sourcePosToPosition, withProcessedDoc)
import Conjure.Language.Lexer (ETok (..), Offsets (..), trueLength)
import Conjure.Language.Validator (TagType (..), TaggedToken (TaggedToken), ValidatorState (symbolCategories))
import Conjure.Prelude
import Control.Lens
import Data.Map qualified as M
import Language.LSP.Protocol.Lens (HasParams (..), HasTextDocument (..))
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server (Handlers, LspM, requestHandler)

semanticTokensHandler :: Handlers (LspM ())
semanticTokensHandler = semanticTokensHandlerFull

semanticTokensHandlerFull :: Handlers (LspM ())
semanticTokensHandlerFull = requestHandler SMethod_TextDocumentSemanticTokensFull $ \req res -> do
  let ps = req ^. params . textDocument
  withProcessedDoc ps $ \(ProcessedFile _ _ (symbolCategories -> ts) _) -> do
    let toks = mapMaybe createSemanticToken (sortOn (\(TaggedToken _ (oTrueStart . offsets -> e)) -> e) $ M.elems ts)
    let sToks = makeSemanticTokens defaultSemanticTokensLegend toks
    r <- case sToks of
      Left txt -> sendErrorMessage txt >> return (Right $ InR Null)
      Right st -> return . Right . InL $ st
    res r

createSemanticToken :: TaggedToken -> Maybe SemanticTokenAbsolute
createSemanticToken (TaggedToken tt tok) =
  uncurry (SemanticTokenAbsolute ln col len) <$> symbolType tt
  where
    Position ln col = sourcePosToPosition (oSourcePos . offsets $ tok)
    len = fromInteger . toInteger . trueLength $ tok
    symbolType :: TagType -> Maybe (SemanticTokenTypes, [SemanticTokenModifiers])
    symbolType s = case s of
      TtType -> Just (SemanticTokenTypes_Type, [SemanticTokenModifiers_DefaultLibrary])
      TtNumber -> Just (SemanticTokenTypes_Number, [])
      TtBool -> Just (SemanticTokenTypes_Regexp, [])
      TtDomain -> Just (SemanticTokenTypes_Class, [SemanticTokenModifiers_Abstract])
      TtEnum -> Just (SemanticTokenTypes_Enum, [SemanticTokenModifiers_Abstract])
      TtEnumMember -> Just (SemanticTokenTypes_EnumMember, [])
      TtRecord -> Just (SemanticTokenTypes_Struct, [])
      TtRecordMember -> Just (SemanticTokenTypes_Property, [SemanticTokenModifiers_Readonly])
      TtUserFunction -> Just (SemanticTokenTypes_Function, [])
      TtFunction -> Just (SemanticTokenTypes_Function, [SemanticTokenModifiers_DefaultLibrary])
      TtAttribute -> Just (SemanticTokenTypes_Variable, [SemanticTokenModifiers_Readonly])
      TtAAC -> Just (SemanticTokenTypes_Interface, [])
      TtVariable -> Just (SemanticTokenTypes_Variable, [])
      TtKeyword -> Just (SemanticTokenTypes_Keyword, [])
      TtQuantifier -> Just (SemanticTokenTypes_Macro, [])
      TtSubKeyword -> Just (SemanticTokenTypes_Modifier, [])
      TtOperator -> Just (SemanticTokenTypes_Operator, [])
      TtLocal -> Just (SemanticTokenTypes_Parameter, [])
      TtOther _ -> Nothing
