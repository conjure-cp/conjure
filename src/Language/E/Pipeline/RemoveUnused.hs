{-# LANGUAGE QuasiQuotes, ViewPatterns #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.E.Pipeline.RemoveUnused where

import Language.E


removeUnused :: (Monad m, Functor m)
    => Spec
    -> CompE m Spec
removeUnused (Spec v statements) = do
    statements' <- forM (withRestToR statements) $ \ (this, afterThis) -> do
        let maybeName = case this of
                            [xMatch| [Prim (S nm)] := topLevel.declaration.find .name.reference |] -> Just nm
                            [xMatch| [Prim (S nm)] := topLevel.declaration.given.name.reference |] -> Just nm
                            [xMatch| [Prim (S nm)] := topLevel.letting          .name.reference |] -> Just nm
                            _ -> Nothing
        case maybeName of
            Nothing   -> do
                -- mkLog "removeUnused" $ "has no name" <+> pretty this
                return (Just this)
            Just name -> do
                -- mkLog "removeUnused" $ "has a name" <+> pretty this
                if name `elem` concatMap identifiersIn afterThis
                    then do
                        -- mkLog "removeUnused" $ "is referenced later" <+> pretty this
                        return (Just this)
                    else do
                        mkLog "removed" $ pretty this
                        return Nothing
    return (Spec v $ catMaybes statements')

identifiersIn :: E -> [String]
identifiersIn e = [ head (splitOn "#" s)
                  | [xMatch| [Prim (S s)] := reference |] <- universe e
                  ]
