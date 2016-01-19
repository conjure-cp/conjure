{-# LANGUAGE TupleSections #-}
{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.Unnameds
    ( removeUnnamedsFromModel
    , addUnnamedStructurals
    ) where

import Conjure.Prelude
import Conjure.Language
import qualified Conjure.Language.ModelStats as ModelStats ( finds )


-- | The argument is a model before nameResolution.
--   Only intended to work on problem specifications.
--   Replaces unnamed types with integers.
removeUnnamedsFromModel :: (MonadFail m, MonadLog m) => Model -> m Model
removeUnnamedsFromModel model = do
    statements' <- forM (mStatements model) $ \ st ->
            case st of
                Declaration (LettingDomainDefnUnnamed name size) -> do
                    let outDomain = mkDomainIntB 1 size
                    return $ Declaration $ Letting name $ Domain outDomain
                _ -> return st
    return model { mStatements = statements' }

addUnnamedStructurals :: (MonadFail m, MonadLog m, NameGen m) => Model -> m Model
addUnnamedStructurals model = do
    -- assuming the info is ready by this point
    let allUnnnameds = model |> mInfo |> miUnnameds
    let allFinds = model |> ModelStats.finds
    -- TODO: the following is too much, a subset of allUnnnameds and allFinds will have to be selected
    cons <- sequence [ mkUnnamedStructuralCons unnamed decVar
                     | (unnamed, decVar) <-
                         [ (unnamed, decVar)
                         | unnamed <- allUnnnameds
                         , decVar  <- allFinds
                         ]
                     ]
    case catMaybes cons of
        []         -> return model
        newConsYay -> return model { mStatements = mStatements model ++ [SuchThat newConsYay] }


mkUnnamedStructuralCons
    :: (MonadFail m, NameGen m)
    => (Name, Expression)
    -> (Name, Domain () Expression)
    -> m (Maybe Expression)
mkUnnamedStructuralCons (unnamedName, unnamedSize) (name, domain) = onDomain domain where
    onDomain (DomainSet _ _ (DomainReference n _)) | n == unnamedName = do
            (iPat , i ) <- quantifiedVar
            (jPat , j ) <- lettingVar
            (k1Pat, k1) <- quantifiedVar
            (k2Pat, k2) <- lettingVar
            let nameExpr = Reference name (Just (DeclNoRepr Find name domain Region_UnnamedSymBreaking))
            return $ Just [essence|
                and([ [ &k1 in &nameExpr          | &k1Pat : int(1..&unnamedSize) ]
                          >=lex
                      [ &k2 in &nameExpr          | &k1Pat : int(1..&unnamedSize)
                                                  , letting &k2Pat be
                                                      $ if k1 = i then j else (if k1 = j then i else k1)
                                                      $ (k1=i) + 2 * (k1=j)          k1=i implies 1    (k2=j)
                                                      $                              k1=j implies 2    (k2=i)
                                                      $                              o.w. implies 0    (k2=k1)
                                                      [ &k1, &j, &i ; int(0..2) ] [ toInt(&k1=&i) + 2 * toInt(&k1=&j) ]
                                                  ]
                    | &iPat : int(1..&unnamedSize - 1)
                    , letting &jPat be &i + 1
                    ])
                           |]
    onDomain (DomainMSet _ _ (DomainReference n _)) | n == unnamedName = do
            (iPat , i ) <- quantifiedVar
            (jPat , j ) <- lettingVar
            (k1Pat, k1) <- quantifiedVar
            (k2Pat, k2) <- lettingVar
            let nameExpr = Reference name (Just (DeclNoRepr Find name domain Region_UnnamedSymBreaking))
            return $ Just [essence|
                and([ [ freq(&nameExpr, &k1)      | &k1Pat : int(1..&unnamedSize) ]
                          >=lex
                      [ freq(&nameExpr, &k2)      | &k1Pat : int(1..&unnamedSize)
                                                  , letting &k2Pat be
                                                      [ &k1, &j, &i ; int(0..2) ] [ toInt(&k1=&i) + 2 * toInt(&k1=&j) ]
                                                  ]
                    | &iPat : int(1..&unnamedSize - 1)
                    , letting &jPat be &i + 1
                    ])
                           |]
    onDomain (DomainFunction _ _ (DomainReference n _) domTo) | n == unnamedName = do
            (iPat , i ) <- quantifiedVar
            (jPat , j ) <- lettingVar
            (k1Pat, k1) <- quantifiedVar
            (k2Pat, k2) <- lettingVar
            (zPat , z ) <- quantifiedVar
            let nameExpr = Reference name (Just (DeclNoRepr Find name domain Region_UnnamedSymBreaking))
            return $ Just [essence|
                and([ [ (&k1, &z) in toSet(&nameExpr)
                                                  | &k1Pat : int(1..&unnamedSize)
                                                  , &zPat  : &domTo
                                                  ]
                          >=lex
                      [ (&k2, &z) in toSet(&nameExpr)
                                                  | &k1Pat : int(1..&unnamedSize)
                                                  , &zPat  : &domTo
                                                  , letting &k2Pat be
                                                      [ &k1, &j, &i ; int(0..2) ] [ toInt(&k1=&i) + 2 * toInt(&k1=&j) ]
                                                  ]
                    | &iPat : int(1..&unnamedSize - 1)
                    , letting &jPat be &i + 1
                    ])
                           |]
    onDomain (DomainFunction _ _ domFr (DomainReference n _)) | n == unnamedName = do
            (iPat , i ) <- quantifiedVar
            (jPat , j ) <- lettingVar
            (k1Pat, k1) <- quantifiedVar
            (k2Pat, k2) <- lettingVar
            (zPat , z ) <- quantifiedVar
            let nameExpr = Reference name (Just (DeclNoRepr Find name domain Region_UnnamedSymBreaking))
            return $ Just [essence|
                and([ [ (&z, &k1) in toSet(&nameExpr)
                                                  | &zPat  : &domFr
                                                  , &k1Pat : int(1..&unnamedSize)
                                                  ]
                          >=lex
                      [ (&z, &k2) in toSet(&nameExpr)
                                                  | &zPat  : &domFr
                                                  , &k1Pat : int(1..&unnamedSize)
                                                  , letting &k2Pat be
                                                      [ &k1, &j, &i ; int(0..2) ] [ toInt(&k1=&i) + 2 * toInt(&k1=&j) ]
                                                  ]
                    | &iPat : int(1..&unnamedSize - 1)
                    , letting &jPat be &i + 1
                    ])
                           |]
    onDomain _ = return Nothing

