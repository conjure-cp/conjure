module Conjure.LSP.Handlers.Suggestions where

import Conjure.LSP.Util (ProcessedFile (ProcessedFile), regionToRange, withProcessedDoc,getNextTokenStart,sourcePosToPosition)
import Conjure.Language (Type (..))
import Conjure.Language.AST.Reformer
import Conjure.Language.Type (IntTag (..))
import Conjure.Language.Lexer
import Conjure.Language.Lexemes
import Conjure.Language.Validator --(Class (..), Kind (..), RegionInfo (..), ValidatorState (regionInfo), RegionType (..), StructuralType (..),symbolTable)
import Conjure.Prelude
import Control.Lens
import Data.Text (intercalate,pack)
import Language.LSP.Server (Handlers, LspM, requestHandler)
import Language.LSP.Types (SymbolKind(..),SMethod(STextDocumentCompletion),type  (|?) (..), CompletionItem (..), CompletionItemKind (..), Position(..))
import qualified Language.LSP.Types as T 
import Language.LSP.Types.Lens (HasParams (..), HasTextDocument (textDocument), HasPosition (position))
import Conjure.Language.Pretty (prettyT)
import qualified Data.Map.Strict as Map
import Conjure.Language.Validator (ErrorType(TokenError), DiagnosticRegion (drSourcePos))
import Conjure.Language.AST.Syntax (LToken(MissingToken))

suggestionHandler :: Handlers (LspM ())
suggestionHandler = requestHandler STextDocumentCompletion $ \req res -> do
    let ps = req ^. params . textDocument
    let context = req ^. params . position
    withProcessedDoc ps $ \(ProcessedFile _ diags (regionInfo -> ri) pt) -> do
        let symbols = Map.toList $ rTable $ head  ri
        let nextTStart = getNextTokenStart context pt
        let errors = [(r,d) | (ValidatorDiagnostic r (Error (TokenError d))) <- diags ] 
        let contextTokens = take 1 [ lexeme w | (r,MissingToken w) <- errors,isInRange nextTStart r] 
        let missingTokenBasedHint = case contextTokens of 
                [l] -> makeMissingTokenHint l
                _ -> []
                where 
                    makeMissingTokenHint (L_Missing s) = [pack s]
                    makeMissingTokenHint (LMissingIdentifier) = ["newIdentifier1"]
                    makeMissingTokenHint l = [lexemeText l]
        res $ Right $ InL . T.List $ (map defaultCompletion missingTokenBasedHint) ++  makeSuggestionsFromSymbolTable symbols

isInRange :: T.Position -> DiagnosticRegion -> Bool
isInRange p reg = sourcePosToPosition (drSourcePos reg) == p


makeSuggestionsFromSymbolTable :: [(Text,SymbolTableValue)] -> [CompletionItem]
makeSuggestionsFromSymbolTable = map symbolToHint

symbolToHint :: (Text,SymbolTableValue) -> CompletionItem
symbolToHint (name,(_,_,k)) = let
    typeName = prettyT k
    in (defaultCompletion name){_detail = Just typeName ,_kind=pure $ getCIKind k}

getCIKind :: Kind -> CompletionItemKind
getCIKind (Kind DomainType _) = CiClass
getCIKind (Kind ValueType{} t) = case t of
    TypeAny -> CiVariable
    TypeBool -> CiVariable
    TypeInt _ -> CiVariable
    TypeEnum _ -> CiEnum
    TypeUnnamed _ -> CiVariable
    TypeTuple _ -> CiVariable
    TypeRecord _ -> CiVariable
    TypeRecordMember _ _ -> CiEnumMember
    TypeVariant _ -> CiVariable
    TypeVariantMember _ _ -> CiEnumMember
    TypeList _ -> CiVariable
    TypeMatrix _ _ -> CiVariable
    TypeSet _ -> CiVariable
    TypeMSet _ -> CiVariable
    TypeFunction _ _ -> CiVariable
    TypeSequence _ -> CiVariable
    TypeRelation _ -> CiVariable
    TypePartition _ -> CiVariable


defaultCompletion :: Text -> CompletionItem
defaultCompletion n = CompletionItem
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

missingToSuggestion :: [CompletionItem]
missingToSuggestion = []

keywordCompletions :: [CompletionItem]
keywordCompletions = []