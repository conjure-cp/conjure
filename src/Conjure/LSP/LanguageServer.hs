{-# LANGUAGE DataKinds #-}

module Conjure.LSP.LanguageServer where

import Language.LSP.Server
import qualified Language.LSP.Types as J

import Conjure.LSP.Handlers.File (fileHandlers, fileOpenedHandler)
import Conjure.LSP.Handlers.Initialize (handleInitialized)
import Conjure.Prelude
import Conjure.LSP.Handlers.Hover (hoverHandler)
import Conjure.LSP.Handlers.DocumentSymbol (docSymbolHandler)
import Conjure.LSP.Handlers.SemanticTokens (semanticTokensHandler)

data LSPConfig = LSPConfig {}

startServer :: LSPConfig -> IO ()
startServer cfg = do
    _ <- runServer $ conjureLanguageServer
    return ()



conjureLanguageServer :: ServerDefinition ()
conjureLanguageServer =
    ServerDefinition
        { onConfigurationChange = const $ pure $ Right ()
        , doInitialize = \env _req -> pure $ Right env
        , staticHandlers = handlers
        , interpretHandler = \env -> Iso (runLspT env) liftIO
        , options = lspOptions
        , defaultConfig = def
        }

handlers :: Handlers (LspM ())
handlers =
    mconcat
        [ fileHandlers
        , handleInitialized
        , hoverHandler
        , docSymbolHandler
        , semanticTokensHandler
        ]


syncOptions :: J.TextDocumentSyncOptions
syncOptions = J.TextDocumentSyncOptions
    (Just True)
    (Just J.TdSyncIncremental)
    (Just False)
    (Just False)
    (Just $ J.InR $ J.SaveOptions $ Just False)
  

lspOptions :: Options
lspOptions = defaultOptions
  { textDocumentSync = Just syncOptions
  , executeCommandCommands = Just ["lsp-hello-command"]
  }