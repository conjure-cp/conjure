module Conjure.LSP.Documentation where

import Conjure.Language.Validator (DocType (..), RegionType (Documentation))
import Conjure.Prelude
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Language.LSP.Protocol.Types (MarkupContent (..), MarkupKind (..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS

getDocsForBuiltin :: RegionType -> IO (Maybe MarkupContent)
getDocsForBuiltin (Documentation prefix (T.unpack -> name)) = do
  let category = case prefix of
        OperatorD -> "operator"
        FunctionD -> "function"
        KeywordD -> "keyword"
        TypeD -> "type"
        AttributeD -> "attribute"

      download = do
        -- create a connection manager
        manager <- newManager tlsManagerSettings
        -- create the request
        request <- parseRequest (readUrl category name)
        -- make the request
        r <- httpLbs request manager
        -- get the contents (as a lazy ByteString)
        let contents = decodeUtf8 $ BL.toStrict $ responseBody r
        if contents == "404: Not Found"
          then return $ Just $ fallbackMsg category name
          else
            return
              $ Just
              $ MarkupContent MarkupKind_Markdown
              $ T.concat
                [ contents,
                  "\n\n -- \n\n",
                  "[Edit this doc](",
                  editURL category name,
                  ")"
                ]

      handler :: HttpException -> IO (Maybe MarkupContent)
      handler _ = return $ Just $ MarkupContent MarkupKind_Markdown "No internet connection"

  download `catch` handler
getDocsForBuiltin _ = pure Nothing

fallbackMsg :: String -> String -> MarkupContent
fallbackMsg c n = MarkupContent MarkupKind_Markdown $ T.concat ["[Create this doc](", createURL c n, ")"]

branch :: String
branch = "main"

readUrl :: String -> String -> String
readUrl category name =
  concat
    [ "https://raw.githubusercontent.com/conjure-cp/conjure/" ++ branch ++ "/docs/bits/",
      category,
      "/",
      name,
      ".md"
    ]

createURL :: String -> String -> Text
createURL category name =
  T.pack
    $ concat
      [ "https://github.com/conjure-cp/conjure/new/" ++ branch ++ "/docs/bits/",
        category,
        "?filename=",
        name,
        ".md"
      ]

editURL :: String -> String -> Text
editURL category name =
  T.pack
    $ concat
      [ "https://github.com/conjure-cp/conjure/edit/" ++ branch ++ "/docs/bits/",
        category,
        "/",
        name,
        ".md"
      ]
