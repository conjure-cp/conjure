{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE DataKinds #-}
module Conjure.LSP.Handlers.Initialize where
import Language.LSP.Types (SMethod(SInitialized, SWindowShowMessage, SInitialize), NotificationMessage (NotificationMessage), ShowMessageParams (ShowMessageParams), MessageType (MtInfo), InitializedParams (InitializedParams), RequestMessage (..), InitializeParams (InitializeParams), Method (Initialize))
import Language.LSP.Server
import Conjure.Prelude


-- handleInitialize :: Handlers (LspM ())
-- handleInitialize = requestHandler SInitialize $ \ req res -> do


handleInitialized :: Handlers (LspM ())
handleInitialized = notificationHandler SInitialized $ \req -> do
    let NotificationMessage _ _ (a) = req
    sendNotification SWindowShowMessage (ShowMessageParams MtInfo "LSP Started 0.0.2\n")
    

