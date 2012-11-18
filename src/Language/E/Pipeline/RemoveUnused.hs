{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.RemoveUnused where

import Language.E


-- TODO: do this better with the new single statement setting!

removeUnused
    :: MonadConjure m
    => Spec
    -> m Spec
removeUnused (Spec v statements) = do
    statements' <- forM (withRestToR $ statementAsList statements) $ \ (this, afterThis) -> do
        let maybeName = case this of
                            [xMatch| [Prim (S nm)] := topLevel.declaration.find .name.reference |] -> Just nm
                            [xMatch| [Prim (S nm)] := topLevel.declaration.given.name.reference |] -> Just nm
                            [xMatch| [Prim (S nm)] := topLevel.letting          .name.reference |] -> Just nm
                            _ -> Nothing
        case maybeName of
            Nothing   -> return (Just this)
            Just name -> do
                let identifiersAfterThis = concatMap identifiersIn afterThis
                if name `elem` identifiersAfterThis
                    then
                        return (Just this)
                    else do
                        mkLog "removedDecl" $ pretty this
                        return Nothing
    return (Spec v $ listAsStatement $ catMaybes statements')

identifiersIn :: E -> [String]
identifiersIn e = [ head (splitOn "#" s)
                  | [xMatch| [Prim (S s)] := reference |] <- universe e
                  ]

