module Conjure.LSP.Handlers.DocumentSymbol where

import Conjure.LSP.Util (ProcessedFile (ProcessedFile), getProcessedDoc, getRelevantRegions, regionToRange, withProcessedDoc)
import Conjure.Language (Type (..))
import Conjure.Language.Pretty (Pretty (..))
import Conjure.Language.Type (IntTag (..))
import Conjure.Language.Validator (Class (..), Kind (..), RegionInfo (..), ValidatorState (regionInfo), RegionType (..), StructuralType (..))
import Conjure.Prelude
import Control.Lens
import Data.Text (pack, unpack, intercalate)
import Language.LSP.Server (Handlers, LspM, requestHandler, sendNotification)
import Language.LSP.Types (SymbolKind(..),SMethod(STextDocumentDocumentSymbol),type  (|?) (..), DocumentSymbol (..))
import qualified Language.LSP.Types as T 
import Language.LSP.Types.Lens (HasParams (..), HasTextDocument (textDocument))
import Conjure.Language.Pretty (prettyT)

docSymbolHandler :: Handlers (LspM ())
docSymbolHandler = requestHandler STextDocumentDocumentSymbol $ \req res -> do
    let ps = req ^. params . textDocument
    withProcessedDoc ps $ \(ProcessedFile _ _ (regionInfo -> ri)) -> do
        res $ Right $ InL . T.List $ mapMaybe translate ri


translate :: RegionInfo -> Maybe T.DocumentSymbol
translate reg@(RegionInfo r rSel ty cs) =
    (\x -> DocumentSymbol
                (getRegionName reg)
                (getRegionDetail reg)
                x
                Nothing
                Nothing
                (regionToRange r)
                (regionToRange (fromMaybe r rSel))
                (Just . T.List $ mapMaybe translate cs)

        ) <$> sk
    where
        sk = symbolKindFromDeclaration ty

getRegionName :: RegionInfo -> Text
getRegionName (rRegionType->rType) = case rType of 
  Definition txt ki -> txt
  LiteralDecl ki -> "Literal"
  Ref txt ki dr -> txt
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
    _ -> pack $ show st
  Documentation dt txt -> ""

getRegionDetail :: RegionInfo -> Maybe Text
getRegionDetail (RegionInfo{rRegionType=rType,rChildren=children}) = 
    case rType of
        Definition txt ki -> Just $ prettyT ki
        LiteralDecl ki -> Just $ prettyT ki
        Ref txt ki dr -> Just $ prettyT ki
        Structural st -> case st of
            SSuchThat -> Nothing
            SGiven -> Just $ getDefs children
            SFind -> Just  $ getDefs children
            SLetting -> Just $ getDefs children
            SEnum nm -> Just "new type enum"
            SQuantification  _ ki -> Just $ prettyT ki
            SComprehension ki -> Just $ prettyT ki
            _ -> Nothing
        Documentation dt txt -> Nothing
    where        
        getDefs :: [RegionInfo] -> Text
        getDefs rs = Data.Text.intercalate ", " [nm | Definition nm _ <- rRegionType <$> rs]
symbolKindFromDeclaration :: RegionType -> Maybe T.SymbolKind
symbolKindFromDeclaration (Definition _ t) = Just $ case t of
    Kind ValueType (TypeInt TagEnum{}) -> SkEnumMember
    Kind ValueType (TypeRecordMember{}) -> SkField
    Kind ValueType (TypeVariantMember{}) -> SkField
    Kind ValueType _ -> SkVariable
    Kind DomainType _ -> SkTypeParameter
symbolKindFromDeclaration (LiteralDecl t) = Just $ case t of
                Kind _ ty -> case ty of
                    TypeBool -> SkBoolean
                    TypeInt it -> case it of
                        TagInt -> SkNumber
                        TagEnum _ -> SkEnumMember
                        TagUnnamed _ -> SkNumber
                    TypeEnum _ -> SkEnum
                    TypeUnnamed _ -> SkEnum
                    _ -> SkConstant
symbolKindFromDeclaration (Structural st) = Just $ (case st of
   SSuchThat -> SkInterface
   SGiven -> SkProperty
   SFind -> SkField
   SLetting -> SkField
   SBranching -> SkClass
   SEnum nm -> SkEnum
   SQuantification _ _-> SkOperator
   SComprehension _ -> SkArray
   SGuard ->  SkBoolean
   SGen ->  SkEvent
   SBody ->  SkNamespace
   SGoal _ -> SkVariable
   SWhere -> SkObject
   )
symbolKindFromDeclaration _ = Nothing