{-# LANGUAGE DataKinds #-}

module Conjure.LSP.LanguageServer where

import Conjure.LSP.Handlers.DocumentSymbol (docSymbolHandler)
import Conjure.LSP.Handlers.File (fileHandlers, unhandled)
import Conjure.LSP.Handlers.Format (formatHandler)
import Conjure.LSP.Handlers.Hover (hoverHandler)
import Conjure.LSP.Handlers.Initialize (handleInitialized)
import Conjure.LSP.Handlers.SemanticTokens (semanticTokensHandler)
import Conjure.LSP.Handlers.Suggestions (suggestionHandler)
import Conjure.Prelude
import Language.LSP.Protocol.Types
import Language.LSP.Server

data LSPConfig = LSPConfig {}

startServer :: LSPConfig -> IO ()
startServer _ = do
  _ <- runServer $ conjureLanguageServer
  return ()

conjureLanguageServer :: ServerDefinition ()
conjureLanguageServer =
  ServerDefinition
    { onConfigChange = const $ pure (),
      doInitialize = \env _req -> pure $ Right env,
      staticHandlers = handlers,
      interpretHandler = \env -> Iso (runLspT env) liftIO,
      options = lspOptions,
      defaultConfig = def,
      configSection = "?",
      parseConfig = \_ _ -> Right ()
    }

handlers :: ClientCapabilities -> Handlers (LspM ())
handlers _ =
  mconcat
    $ unhandled
    ++ [ fileHandlers,
         handleInitialized,
         hoverHandler,
         docSymbolHandler,
         semanticTokensHandler,
         formatHandler,
         suggestionHandler
       ]

syncOptions :: TextDocumentSyncOptions
syncOptions =
  TextDocumentSyncOptions
    (Just True)
    (Just TextDocumentSyncKind_Incremental)
    (Just False)
    (Just False)
    (Just $ InR $ SaveOptions $ Just False)

lspOptions :: Options
lspOptions =
  defaultOptions
    { optTextDocumentSync = Just syncOptions,
      optExecuteCommandCommands = Just ["lsp-hello-command"]
    }