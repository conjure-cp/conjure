module Conjure.LSP.Documentation where
import Paths_conjure_cp (getDataDir, getDataFileName)
import Language.LSP.Server (LspM)
import Language.LSP.Types (MarkedString, MarkupKind (MkMarkdown), MarkupContent (MarkupContent), markedUpContent, unmarkedUpContent)
import Conjure.Prelude
import Language.LSP.Types.Lens (HasRootPath(rootPath))
import qualified Data.Text as T
import Conjure.Language.Validator (DeclarationType (BuiltIn), DocType (..))

tryGetDocsByName :: String -> IO(Maybe MarkupContent)
tryGetDocsByName name = do
    fileName <-  getDataFileName ("data/docs/"++name ++ ".md") 
    fileData <- readFileIfExists fileName
    return $ MarkupContent MkMarkdown . T.pack  <$> fileData
 

getDocsForBuiltin :: DeclarationType -> IO (Maybe MarkupContent)
getDocsForBuiltin (BuiltIn prefix (T.unpack->name)) = do
    let category = case prefix of
          OperatorD -> "op/"
          FunctionD -> "function/"
          KeywordD -> "keyword/"
    res <- tryGetDocsByName $ category ++ name
    return . Just $ case res of 
      Nothing -> fallbackMsg category name
      Just mc -> mc
getDocsForBuiltin _ = pure Nothing


fallbackMsg :: String -> String -> MarkupContent
fallbackMsg c n = MarkupContent MkMarkdown $ T.concat ["[Create This Doc](",getEditUrl c n,")"]

getEditUrl :: String -> String -> Text
getEditUrl category name = T.pack $ concat [
            "https://github.com/conjure-cp/conjure/new/master/data/docs/"
             ,category
             ,name
             ,"?filename="
             , name
             ,".md"
            ]