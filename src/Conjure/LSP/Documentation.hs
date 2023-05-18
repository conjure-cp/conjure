module Conjure.LSP.Documentation where
import Paths_conjure_cp (getDataFileName)
import Language.LSP.Types (MarkupKind (MkMarkdown), MarkupContent (MarkupContent))
import Conjure.Prelude
import qualified Data.Text as T
import Conjure.Language.Validator (DocType (..), RegionType (Documentation))

tryGetDocsByName :: String -> IO(Maybe MarkupContent)
tryGetDocsByName name = do
    fileName <-  getDataFileName ("data/docs/"++name ++ ".md") 
    fileData <- readFileIfExists fileName
    return $ MarkupContent MkMarkdown . T.pack  <$> fileData
 

getDocsForBuiltin :: RegionType -> IO (Maybe MarkupContent)
getDocsForBuiltin (Documentation prefix (T.unpack->name)) = do
    let category = case prefix of
          OperatorD -> "op/"
          FunctionD -> "function/"
          KeywordD -> "keyword/"
          TypeD -> "types/"
          AttributeD -> "attributes/"
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