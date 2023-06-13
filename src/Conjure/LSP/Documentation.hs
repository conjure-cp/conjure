module Conjure.LSP.Documentation where

import Conjure.Prelude
import Conjure.Language.Validator (DocType (..), RegionType (Documentation))

import Language.LSP.Types (MarkupKind (MkMarkdown), MarkupContent (MarkupContent))
import qualified Data.Text as T
import Data.Text.Encoding ( decodeUtf8 )

-- from: https://stackoverflow.com/a/60793739/463977
import Network.HTTP.Client          -- package http-client
import Network.HTTP.Client.TLS      -- package http-client-tls
import qualified Data.ByteString.Lazy as BL

getDocsForBuiltin :: RegionType -> IO (Maybe MarkupContent)
getDocsForBuiltin (Documentation prefix (T.unpack -> name)) = do
    let category = case prefix of
          OperatorD -> "operator"
          FunctionD -> "function"
          KeywordD -> "keyword"
          TypeD -> "type"
          AttributeD -> "attribute"
    -- create a connection manager
    manager <- newManager tlsManagerSettings
    -- create the request
    request <- parseRequest (getReadUrl category name)
    -- make the request
    r <- httpLbs request manager
    -- get the contents (as a lazy ByteString)
    let contents = decodeUtf8 $ BL.toStrict $ responseBody r

    if contents == "404: Not Found"
        then return $ Just $ fallbackMsg category name
        else return $ Just $ MarkupContent MkMarkdown $ T.concat
            [ contents
            , "\n\n -- \n\n"
            , "[Edit this doc](",edit category name,")"
            ]
getDocsForBuiltin _ = pure Nothing 


fallbackMsg :: String -> String -> MarkupContent
fallbackMsg c n = MarkupContent MkMarkdown $ T.concat ["[Create this doc](", createURL c n,")"]


readUrl :: String -> String -> String
readUrl category name = concat
    [ "https://raw.githubusercontent.com/conjure-cp/conjure/onlinedocs-hover/docs/bits/"
    , category
    , "/"
    , name
    , ".md"
    ]


createURL :: String -> String -> Text
createURL category name = T.pack $ concat [
            "https://github.com/conjure-cp/conjure/new/onlinedocs-hover/docs/bits/"
             ,category
             ,"?filename="
             , name
             ,".md"
            ]


editURL :: String -> String -> Text
editURL category name = T.pack $ concat [
            "https://github.com/conjure-cp/conjure/edit/onlinedocs-hover/docs/bits/"
             ,category
             ,"/"
             , name
             ,".md"
            ]
