module Conjure.LSP.Handlers.Suggestions (suggestionHandler) where

import Conjure.LSP.Util (ProcessedFile (ProcessedFile), getNextTokenStart, getRelevantRegions, positionToSourcePos, sourcePosToPosition, withProcessedDoc)
import Conjure.Language (Type (..))
import Conjure.Language.AST.Reformer
-- (Class (..), Kind (..), RegionInfo (..), ValidatorState (regionInfo), RegionType (..), StructuralType (..),symbolTable)

import Conjure.Language.AST.Syntax (LToken (MissingToken))
import Conjure.Language.Lexemes
import Conjure.Language.Lexer
import Conjure.Language.Pretty (prettyT)
import Conjure.Language.Validator
import Conjure.Prelude
import Control.Lens
import Data.Map.Strict qualified as Map
import Language.LSP.Protocol.Lens (HasParams (..), HasPosition (position), HasTextDocument (textDocument))
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types (CompletionItem (..), CompletionItemKind (..), type (|?) (..))
import Language.LSP.Protocol.Types qualified as T
import Language.LSP.Server (Handlers, LspM, requestHandler)
import Text.Megaparsec (SourcePos)

suggestionHandler :: Handlers (LspM ())
suggestionHandler = requestHandler SMethod_TextDocumentCompletion $ \req res -> do
  let ps = req ^. params . textDocument
  let context = req ^. params . position
  withProcessedDoc ps $ \(ProcessedFile _ diags valState pt) -> do
    let nextTStart = getNextTokenStart context pt
    let roi = getRelevantRegions valState nextTStart
    let innermostSymbolTable = if null roi then [] else Map.toList . rTable $ last roi
    let errors = [(r, d) | (ValidatorDiagnostic r (Error (TokenError d))) <- diags]
    let contextTokens = take 1 [lexeme w | (r, MissingToken w) <- errors, isInRange nextTStart r]
    let missingTokenBasedHint = missingToSuggestion innermostSymbolTable contextTokens
    -- sendInfoMessage $ pack . show $ context
    let tlSymbol = getLowestLevelTaggedRegion (positionToSourcePos context) $ makeTree pt
    let tlSymbolSuggestion = case tlSymbol of
          Just (TIDomain _) -> makeDomainSuggestions innermostSymbolTable
          Just (TIExpression _) -> makeExpressionSuggestions innermostSymbolTable
          Just (TIList t) -> makeTagSuggestions innermostSymbolTable t
          _ -> [] -- or for debugging -> [defaultCompletion $ pack . show $ tlSymbol]
    res
      $ Right
      $ InL
      . nubBy isSameInsertion
      $ concat
        [ missingTokenBasedHint,
          tlSymbolSuggestion,
          keywordCompletions
        ]

isSameInsertion :: CompletionItem -> CompletionItem -> Bool
isSameInsertion CompletionItem {_label = a} CompletionItem {_label = b} = a == b

isInRange :: T.Position -> DiagnosticRegion -> Bool
isInRange p reg = sourcePosToPosition (drSourcePos reg) == p

makeSuggestionsFromSymbolTable :: [(Text, SymbolTableValue)] -> [CompletionItem]
makeSuggestionsFromSymbolTable = map symbolToHint

makeDomainSuggestions :: [(Text, SymbolTableValue)] -> [CompletionItem]
makeDomainSuggestions table = stDomains ++ newDomainPlaceholders
  where
    stDomains = map symbolToHint $ [x | x@(_, (_, _, Kind DomainType t)) <- table, typesUnifyS [t, TypeAny]]
    newDomainPlaceholders =
      uncurry snippetCompletion
        <$> [ ("int", "int"),
              ("int", "bool"),
              ("matrix", "matrix indexed by ${1:[index_domains]} of ${2:type}"),
              ("set", "set of $1"),
              ("mset", "mset of $1")
            ]

makeExpressionSuggestions :: [(Text, SymbolTableValue)] -> [CompletionItem]
makeExpressionSuggestions table = stExprs ++ newExpressionPlaceholders
  where
    stExprs = map symbolToHint $ [x | x@(_, (_, _, Kind ValueType {} t)) <- table, typesUnifyS [t, TypeAny]]
    newExpressionPlaceholders = []

makeTagSuggestions :: [(Text, SymbolTableValue)] -> ListItemClasses -> [CompletionItem]
makeTagSuggestions table tag = case tag of
  ICAttribute -> defaultCompletion <$> ["size"]
  ICExpression -> makeExpressionSuggestions table
  ICDomain -> makeDomainSuggestions table
  ICRange -> uncurry snippetCompletion <$> [("openL", "..$1"), ("closed", "$1..$2"), ("openR", "$1..")]
  ICIdentifier -> freeIdentifierSuggestion table
  ICStatement -> topLevelSuggestions

symbolToHint :: (Text, SymbolTableValue) -> CompletionItem
symbolToHint (name, (_, _, k)) =
  let typeName = prettyT k
   in (defaultCompletion name) {_detail = Just typeName, _kind = pure $ getCIKind k}

getCIKind :: Kind -> CompletionItemKind
getCIKind (Kind DomainType _) = CompletionItemKind_Class
getCIKind (Kind ValueType {} t) = case t of
  TypeAny -> CompletionItemKind_Variable
  TypeBool -> CompletionItemKind_Variable
  TypeInt _ -> CompletionItemKind_Variable
  TypeEnum _ -> CompletionItemKind_Enum
  TypeUnnamed _ -> CompletionItemKind_Variable
  TypeTuple _ -> CompletionItemKind_Variable
  TypeRecord _ -> CompletionItemKind_Variable
  TypeRecordMember _ _ -> CompletionItemKind_EnumMember
  TypeVariant _ -> CompletionItemKind_Variable
  TypeVariantMember _ _ -> CompletionItemKind_EnumMember
  TypeList _ -> CompletionItemKind_Variable
  TypeMatrix _ _ -> CompletionItemKind_Variable
  TypeSet _ -> CompletionItemKind_Variable
  TypeMSet _ -> CompletionItemKind_Variable
  TypeFunction _ _ -> CompletionItemKind_Variable
  TypeSequence _ -> CompletionItemKind_Variable
  TypeRelation _ -> CompletionItemKind_Variable
  TypePartition _ -> CompletionItemKind_Variable
  TypePermutation{} -> CompletionItemKind_Variable

snippetCompletion :: Text -> Text -> CompletionItem
snippetCompletion label snippet = (defaultCompletion label) {_kind = pure CompletionItemKind_Snippet, _insertText = pure snippet, _insertTextFormat = pure T.InsertTextFormat_Snippet}

defaultCompletion :: Text -> CompletionItem
defaultCompletion n =
  CompletionItem
    n
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing
    Nothing

--
missingToSuggestion :: [(Text, SymbolTableValue)] -> [Lexeme] -> [CompletionItem]
missingToSuggestion table (x : _) = makeMissingTokenHint x
  where
    makeMissingTokenHint (L_Missing s) = case s of
      MissingExpression -> makeExpressionSuggestions table
      MissingDomain -> makeDomainSuggestions table
      MissingUnknown -> []
    makeMissingTokenHint LMissingIdentifier = freeIdentifierSuggestion table
    makeMissingTokenHint l = [defaultCompletion $ lexemeText l]
missingToSuggestion table _ = makeSuggestionsFromSymbolTable table

keywordCompletions :: [CompletionItem]
keywordCompletions = []

getLowestLevelTaggedRegion :: SourcePos -> HLTree -> Maybe TreeItemLinks
getLowestLevelTaggedRegion p tr =
  let regs = filterContaining p tr
   in case [t | HLTagged t _ <- regs, t /= TIGeneral] of
        [] -> Nothing
        ins -> Just $ last ins

topLevelSuggestions :: [CompletionItem]
topLevelSuggestions =
  uncurry snippetCompletion
    <$> [ ("find", "find $1 : $2"),
          ("given", "such that $0"),
          ("such that", "given $1 : $2")
        ]

freeIdentifierSuggestion :: a -> [CompletionItem]
freeIdentifierSuggestion _ = [defaultCompletion "identifier"]