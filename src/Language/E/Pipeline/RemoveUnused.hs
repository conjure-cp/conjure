{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.RemoveUnused where

import Language.E

import qualified Data.HashSet as S


-- TODO: do this better with the new single statement setting!

removeUnused
    :: MonadConjure m
    => Spec
    -> m Spec
removeUnused (Spec v statements) = Spec v <$> go statements
    where
        go [xMatch| [this] := statement.this
                  | [next] := statement.next
                  |] = do
            let maybeName = case this of
                    [xMatch| [Prim (S nm)] := topLevel.declaration.find .name.reference
                           | [d]           := topLevel.declaration.find .domain
                           |] | domainNeedsRepresentation d -> Just nm
                    [xMatch| [Prim (S nm)] := topLevel.declaration.given.name.reference
                           | [d]           := topLevel.declaration.given.domain
                           |] | domainNeedsRepresentation d -> Just nm
                    _ -> Nothing
            case maybeName of
                Nothing -> do
                    next' <- go next
                    return [xMake| statement.this := [this]
                                 | statement.next := [next']
                                 |]
                Just name -> do
                    next' <- go next
                    if name `S.member` identifiersIn next
                        then return [xMake| statement.this := [this]
                                          | statement.next := [next']
                                          |]
                        else do
                            mkLog "removedDecl" (pretty this)
                            return next'
        go p = return p

identifiersIn :: E -> S.HashSet Text
identifiersIn e =
    S.fromList [ base
               | [xMatch| [Prim (S s)] := reference |] <- universe e
               , let (base, _, _) = identifierSplit s
               ]

