{-# LANGUAGE DataKinds #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE TypeFamilies #-}

module Conjure.LSP.Handlers.Initialize where

import Conjure.Prelude
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server

handleInitialized :: Handlers (LspM ())
handleInitialized = notificationHandler SMethod_Initialized $ \_ -> do
  sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Info "Conjure LSP started\n")
