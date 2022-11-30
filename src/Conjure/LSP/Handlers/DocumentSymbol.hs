module Conjure.LSP.Handlers.DocumentSymbol where
import Language.LSP.Server (requestHandler, LspM, Handlers, sendNotification)
import Language.LSP.Types as T
import Control.Lens
import Conjure.Prelude
import Data.Text (pack,unpack)
import Conjure.LSP.Util (getProcessedDoc, getRelevantRegions, withProcessedDoc, ProcessedFile (ProcessedFile), regionToRange)
import Conjure.Language.Validator (RegionInfo (..), ValidatorState (regionInfo), DeclarationType (..), Kind (..), Class (..))
import Language.LSP.Types.Lens (HasParams(..), HasTextDocument (textDocument))
import Conjure.Language (Type(..))
import Conjure.Language.Type (IntTag(..))
import Conjure.Language.Pretty (Pretty(..))

docSymbolHandler :: Handlers (LspM ())
docSymbolHandler = requestHandler STextDocumentDocumentSymbol $ \ req res -> do
    let ps = req^.params . textDocument
    let u =  (case ps of { (TextDocumentIdentifier uri) -> uri }) :: Uri
    withProcessedDoc ps $ \(ProcessedFile _ _ (regionInfo->ri)) -> do
        res $ Right $ InR . T.List $ mapMaybe (regionInfoToDocumentSymbols u) ri





regionInfoToDocumentSymbols ::Uri -> RegionInfo -> Maybe SymbolInformation
regionInfoToDocumentSymbols uri (RegionInfo {rRegion=reg,rText=n, rType=t, rDeclaration=dec}) = SymbolInformation
    (pack $  (unpack n) ++":" ++ show  (pretty t))  --Name
    <$>
    sk--Kind
    <*> pure
    (Just (T.List []))
    <*> pure
    Nothing
    <*> pure
    (Location uri (regionToRange reg))
    <*> pure
    Nothing
    where 
        sk = case dec of          
          Definition -> Just $ case t of 
                    Kind ValueType _ -> SkVariable
                    Kind DomainType _ -> SkClass
                    Kind MemberType _ -> SkEnumMember --DO better
          LiteralDecl -> do
            case t of 
              Kind _ ty -> return $ case ty of 
                TypeBool -> SkBoolean
                TypeInt it -> case it of
                  TagInt -> SkNumber
                  TagEnum txt -> SkEnumMember
                  TagUnnamed txt -> SkNumber
                TypeEnum na -> SkEnum
                TypeUnnamed na -> SkEnum
                _ -> SkConstant
             
          Ref _ -> Just $ case t of 
                    Kind ValueType _ -> SkVariable
                    Kind DomainType _ -> SkClass
                    Kind MemberType _ -> SkEnumMember --DO better
