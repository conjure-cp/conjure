module Conjure.LSP.Handlers.DocumentSymbol where

import Conjure.LSP.Util (ProcessedFile (ProcessedFile), regionToRange, withProcessedDoc)
import Conjure.Language (Type (..))
import Conjure.Language.Pretty (prettyT)
import Conjure.Language.Type (IntTag (..))
import Conjure.Language.Validator (Class (..), Kind (..), RegionInfo (..), RegionType (..), StructuralType (..), ValidatorState (regionInfo))
import Conjure.Prelude
import Control.Lens
import Data.Text (intercalate)
import Language.LSP.Protocol.Lens (HasParams (..), HasTextDocument (textDocument))
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types (DocumentSymbol (..), SymbolKind (..), type (|?) (..))
import Language.LSP.Protocol.Types qualified as T
import Language.LSP.Server (Handlers, LspM, requestHandler)

docSymbolHandler :: Handlers (LspM ())
docSymbolHandler = requestHandler SMethod_TextDocumentDocumentSymbol $ \req res -> do
  let ps = req ^. params . textDocument
  withProcessedDoc ps $ \(ProcessedFile _ _ (regionInfo -> ri) _) -> do
    res $ Right $ InR $ InL $ mapMaybe translate ri

translate :: RegionInfo -> Maybe T.DocumentSymbol
translate reg@(RegionInfo r rSel ty cs _) =
  ( \x ->
      DocumentSymbol
        (getRegionName reg)
        (getRegionDetail reg)
        x
        Nothing
        Nothing
        (regionToRange r)
        (regionToRange (fromMaybe r rSel))
        (Just $ mapMaybe translate cs)
  )
    <$> sk
  where
    sk = symbolKindFromDeclaration ty

getRegionName :: RegionInfo -> Text
getRegionName (rRegionType -> rType) = case rType of
  Definition txt _ -> txt
  LiteralDecl _ -> "Literal"
  Ref txt _ _ -> txt
  Structural st -> case st of
    SSuchThat -> "Constraints"
    SGiven -> "Parameters"
    SFind -> "Decision Variables"
    SLetting -> "Definitions"
    SEnum txt -> txt
    SBranching -> "Branch"
    SQuantification n _ -> "Quantification: " `mappend` n
    SComprehension _ -> "Comprehension"
    SBody -> "Body"
    SGuard -> "Guard"
    SGen -> "Generator"
    SWhere -> "Parameter validation"
    SGoal dir -> dir
  Documentation _ _ -> ""

getRegionDetail :: RegionInfo -> Maybe Text
getRegionDetail (RegionInfo {rRegionType = rType, rChildren = childDefs}) =
  case rType of
    Definition _ ki -> Just $ prettyT ki
    LiteralDecl ki -> Just $ prettyT ki
    Ref _ ki _ -> Just $ prettyT ki
    Structural st -> case st of
      SSuchThat -> Nothing
      SGiven -> Just $ getDefs childDefs
      SFind -> Just $ getDefs childDefs
      SLetting -> Just $ getDefs childDefs
      SEnum _ -> Just "new type enum"
      SQuantification _ ki -> Just $ prettyT ki
      SComprehension ki -> Just $ prettyT ki
      _ -> Nothing
    Documentation {} -> Nothing
  where
    getDefs :: [RegionInfo] -> Text
    getDefs rs = Data.Text.intercalate ", " [nm | Definition nm _ <- rRegionType <$> rs]

symbolKindFromDeclaration :: RegionType -> Maybe T.SymbolKind
symbolKindFromDeclaration (Definition _ t) = Just $ case t of
  Kind ValueType {} (TypeInt TagEnum {}) -> SymbolKind_EnumMember
  Kind ValueType {} (TypeRecordMember {}) -> SymbolKind_Field
  Kind ValueType {} (TypeVariantMember {}) -> SymbolKind_Field
  Kind ValueType {} _ -> SymbolKind_Variable
  Kind DomainType _ -> SymbolKind_TypeParameter
symbolKindFromDeclaration (LiteralDecl t) = Just $ case t of
  Kind _ ty -> case ty of
    TypeBool -> SymbolKind_Boolean
    TypeInt it -> case it of
      TagInt -> SymbolKind_Number
      TagEnum _ -> SymbolKind_EnumMember
      TagUnnamed _ -> SymbolKind_Number
    TypeEnum _ -> SymbolKind_Enum
    TypeUnnamed _ -> SymbolKind_Enum
    _ -> SymbolKind_Constant
symbolKindFromDeclaration (Structural st) =
  Just
    ( case st of
        SSuchThat -> SymbolKind_Interface
        SGiven -> SymbolKind_Property
        SFind -> SymbolKind_Field
        SLetting -> SymbolKind_Field
        SBranching -> SymbolKind_Class
        SEnum _ -> SymbolKind_Enum
        SQuantification _ _ -> SymbolKind_Operator
        SComprehension _ -> SymbolKind_Array
        SGuard -> SymbolKind_Boolean
        SGen -> SymbolKind_Event
        SBody -> SymbolKind_Namespace
        SGoal _ -> SymbolKind_Variable
        SWhere -> SymbolKind_Object
    )
symbolKindFromDeclaration _ = Nothing