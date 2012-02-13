{-# LANGUAGE BangPatterns #-}

module Phases.AuxRename where

import Control.Monad.IO.Class ( MonadIO, liftIO )
import Data.Generics.Uniplate.Direct ( transformBi )
import Data.IORef ( newIORef, readIORef, writeIORef )
import Data.List ( isPrefixOf )
import Data.Maybe ( fromMaybe )

import Language.Essence


auxRename :: MonadIO m => Spec -> m Spec
auxRename sp = do
    let
        allBindings = [ nm | (_,nm,_) <- topLevelBindings sp ]
        auxs        = [ nm | nm <- allBindings, "AUX_" `isPrefixOf` nm ]

    counter <- liftIO $ newIORef (0 :: Int)

    let
        nextName :: MonadIO m => m String
        nextName = liftIO $ do
            !i <- readIORef counter
            writeIORef counter (i+1)
            let nm = "aux" ++ show i
            if nm `elem` allBindings
                then nextName
                else return nm

    auxsNew <- mapM (\ a -> do b <- nextName; return (a,b) ) auxs

    let
        f (Identifier nm) = case nm `lookup` auxsNew of
                                Nothing  -> Identifier nm
                                Just nm' -> Identifier nm'
        f t = t

    return $ transformBi f sp { topLevelBindings = [ (bEnum,nm',dom)
                                                   | (bEnum,nm,dom) <- topLevelBindings sp
                                                   , let nm' = fromMaybe nm (lookup nm auxsNew)
                                                   ] }
