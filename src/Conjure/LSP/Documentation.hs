module Conjure.LSP.Documentation where

import Conjure.Language.Validator (RegionType (Documentation))
import Conjure.Prelude
import Data.ByteString.Lazy qualified as BL
import Data.Text qualified as T
import Data.Text.Encoding (decodeUtf8)
import Language.LSP.Protocol.Types (MarkupContent (..), MarkupKind (..))
import Network.HTTP.Client
import Network.HTTP.Client.TLS

getDocsForBuiltin :: RegionType -> IO (Maybe MarkupContent)
getDocsForBuiltin (Documentation (T.unpack -> name)) = do
  let
      download = do
        -- create a connection manager
        manager <- newManager tlsManagerSettings
        -- create the request
        request <- parseRequest (readUrl name)
        -- make the request
        r <- httpLbs request manager
        -- get the contents (as a lazy ByteString)
        let contents = decodeUtf8 $ BL.toStrict $ responseBody r
        if contents == "404: Not Found"
          then return $ Just $ fallbackMsg name
          else
            return
              $ Just
              $ MarkupContent MarkupKind_Markdown
              $ T.concat
                [ contents,
                  "\n\n -- \n\n",
                  "[Edit this doc](",
                  editURL name,
                  ")"
                ]

      handler :: HttpException -> IO (Maybe MarkupContent)
      handler _ = return $ Just $ MarkupContent MarkupKind_Markdown "No internet connection"

  download `catch` handler

getDocsForBuiltin _ = pure Nothing

fallbackMsg :: String -> MarkupContent
fallbackMsg n = MarkupContent MarkupKind_Markdown $ T.concat ["[Create this doc](", createURL n, ")"]

branch :: String
branch = "main"

readUrl :: String -> String
readUrl name =
  concat
    [ "https://raw.githubusercontent.com/conjure-cp/conjure/" ++ branch ++ "/docs/bits/",
      name,
      ".md"
    ]

createURL :: String -> Text
createURL name =
  T.pack
    $ concat
      [ "https://github.com/conjure-cp/conjure/new/" ++ branch ++ "/docs/bits?filename=",
        name,
        ".md"
      ]

editURL :: String -> Text
editURL name =
  T.pack
    $ concat
      [ "https://github.com/conjure-cp/conjure/edit/" ++ branch ++ "/docs/bits/",
        name,
        ".md"
      ]
