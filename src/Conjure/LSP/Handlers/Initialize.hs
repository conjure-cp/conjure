{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Conjure.LSP.Handlers.Initialize where
import Language.LSP.Types (SMethod(SInitialized, SWindowShowMessage), ShowMessageParams (ShowMessageParams), MessageType (MtInfo))
import Language.LSP.Server
import Conjure.Prelude


-- handleInitialize :: Handlers (LspM ())
-- handleInitialize = requestHandler SInitialize $ \ req res -> do


handleInitialized :: Handlers (LspM ())
handleInitialized = notificationHandler SInitialized $ \_ -> do
    sendNotification SWindowShowMessage (ShowMessageParams MtInfo "LSP Started 0.0.2\n")
    

