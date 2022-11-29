module Conjure.LSP.Handlers.DocumentSymbol where
import Language.LSP.Server (requestHandler, LspM, Handlers, sendNotification)
import Language.LSP.Types as T
import Control.Lens
import Conjure.Prelude
import Data.Text (pack,unpack)
import Conjure.LSP.Util (getProcessedDoc, getRelevantRegions, withProcessedDoc, ProcessedFile (ProcessedFile), regionToRange)
import Conjure.Language.Validator (RegionInfo (..), ValidatorState (regionInfo), DeclarationType (..))
import Language.LSP.Types.Lens (HasParams(..), HasTextDocument (textDocument))
import Conjure.Language (Type(..))
import Conjure.Language.Type (IntTag(..))
import Conjure.Language.Pretty (Pretty(..))

docSymbolHandler :: Handlers (LspM ())
docSymbolHandler = requestHandler STextDocumentDocumentSymbol $ \ req res -> do
    let ps = req^.params . textDocument
    let u =  (case ps of { (TextDocumentIdentifier uri) -> uri }) :: Uri
    withProcessedDoc ps $ \(ProcessedFile _ _ (regionInfo->ri)) -> do
        res $ Right $ InR . T.List $ map (regionInfoToDocumentSymbols u) ri





regionInfoToDocumentSymbols ::Uri -> RegionInfo -> SymbolInformation
regionInfoToDocumentSymbols uri (RegionInfo {rRegion=reg,rText=n, rType=t, rDeclaration=dec}) = SymbolInformation
    (pack $  (unpack n) ++":" ++ show  (pretty t))  --Name
    sk--Kind
    (Just (T.List []))
    Nothing
    (Location uri (regionToRange reg)) 
    Nothing
    where 
        sk = case dec of          
          Definition -> SkVariable
          LiteralDecl -> case t of 
            TypeBool -> SkBoolean
            TypeInt it -> case it of
              TagInt -> SkNumber
              TagEnum txt -> SkEnumMember
              TagUnnamed txt -> SkNumber
            TypeEnum na -> SkEnum
            TypeUnnamed na -> SkEnum
            _ -> SkConstant
          Ref _ -> SkVariable
