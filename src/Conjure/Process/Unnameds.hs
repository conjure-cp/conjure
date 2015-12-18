{-# LANGUAGE TupleSections #-}

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
                     | unnamed <- allUnnnameds
                     , decVar  <- allFinds
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
            (k2Pat, k2) <- quantifiedVar
            let nameExpr = Reference name Nothing
            return $ Just [essence|
                and([ [ &k1 in &nameExpr | &k1Pat : int(1..&unnamedSize) ]
                          >=lex
                      [ &k2 in &nameExpr | &k1Pat : int(1..&unnamedSize)
                                         , &k2Pat : int(1..&unnamedSize)
                                         , &k1 = &i               -> &k2 = &j
                                         , &k1 = &j               -> &k2 = &i
                                         , &k1 != &i /\ &k1 != &j -> &k2 = &k1
                                         ]
                    | &iPat : int(1..&unnamedSize - 1)
                    , letting &jPat be &i + 1
                    ])
                           |]
    onDomain _ = return Nothing

