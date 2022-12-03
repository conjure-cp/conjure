module Conjure.LSP.Handlers.DocumentSymbol where

import Conjure.LSP.Util (ProcessedFile (ProcessedFile), getProcessedDoc, getRelevantRegions, regionToRange, withProcessedDoc)
import Conjure.Language (Type (..))
import Conjure.Language.Pretty (Pretty (..))
import Conjure.Language.Type (IntTag (..))
import Conjure.Language.Validator (Class (..), DeclarationType (..), Kind (..), RegionInfo (..), ValidatorState (regionInfo))
import Conjure.Prelude
import Control.Lens
import Data.Text (pack, unpack)
import Language.LSP.Server (Handlers, LspM, requestHandler, sendNotification)
import Language.LSP.Types as T
import Language.LSP.Types.Lens (HasParams (..), HasTextDocument (textDocument))

docSymbolHandler :: Handlers (LspM ())
docSymbolHandler = requestHandler STextDocumentDocumentSymbol $ \req res -> do
    let ps = req ^. params . textDocument
    let u = (case ps of (TextDocumentIdentifier uri) -> uri) :: Uri
    withProcessedDoc ps $ \(ProcessedFile _ _ (regionInfo -> ri)) -> do
        res $ Right $ InR . T.List $ mapMaybe (regionInfoToDocumentSymbols u) ri

regionInfoToDocumentSymbols :: Uri -> RegionInfo -> Maybe SymbolInformation
regionInfoToDocumentSymbols uri (RegionInfo{rRegion = reg, rText = n, rType = t, rDeclaration = dec}) =
    SymbolInformation
        (pack $ (unpack n) ++ ":" ++ show (pretty t)) -- Name
        <$> sk -- Kind
        <*> pure
            (Just (T.List []))
        <*> pure
            Nothing
        <*> pure
            (Location uri (regionToRange reg))
        <*> pure
            Nothing
  where
    sk = symbolKindFromDeclaration dec t

symbolKindFromDeclaration :: DeclarationType -> Maybe Kind -> Maybe SymbolKind
symbolKindFromDeclaration Definition (Just t) = Just $ case t of
    Kind ValueType _ -> SkVariable
    Kind DomainType _ -> SkClass
symbolKindFromDeclaration LiteralDecl (Just t) = Just $ case t of
                Kind _ ty -> case ty of
                    TypeBool -> SkBoolean
                    TypeInt it -> case it of
                        TagInt -> SkNumber
                        TagEnum _ -> SkEnumMember
                        TagUnnamed _ -> SkNumber
                    TypeEnum _ -> SkEnum
                    TypeUnnamed _ -> SkEnum
                    _ -> SkConstant
symbolKindFromDeclaration _ _ = Nothing