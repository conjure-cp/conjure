module Conjure.LSP.Util where
import System.IO (Handle)

import Language.LSP.Test
import Language.LSP.Server
import UnliftIO.Async
import Language.LSP.Types
import Conjure.Prelude
import Conjure.LSP.LanguageServer
import GHC.Conc (forkIO)
import Control.Concurrent (killThread)
import Control.Exception (bracket)
import UnliftIO.Process (createPipe)

-- -- Adapted rom https://github.com/haskell/lsp/blob/master/lsp-test/test/DummyServer.hs
-- withDummyServer :: ((Handle, Handle) -> IO ()) -> IO ()
-- withDummyServer f = do
--   (hinRead, hinWrite) <- createPipe
--   (houtRead, houtWrite) <- createPipe
--   bracket
--     (forkIO $ void $ runServerWithHandles mempty mempty hinRead houtWrite conjureLanguageServer)
--     killThread
--     (const $ f (hinWrite, houtRead))





-- doWithSession :: Session () ->  (Handle,Handle) -> IO ()
-- doWithSession session (hin,hout) = runSessionWithHandles hin hout def fullCaps "." session