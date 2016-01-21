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

    var = Reference name (Just (DeclNoRepr Find name domain Region_UnnamedSymBreaking))

    -- current=i then j
    -- current=j then i
    -- otherwise      current
    swapIJ i j current =
        -- if current=i then j else (if current=j then i else current)
        -- (current=i) + 2 * (current=j)    current=i ----> 1    (output is j)
        --                                  current=j ----> 2    (output is i)
        --                                  otherwise ----> 0    (output is current)
        [essence| [ &current, &j, &i ; int(0..2) ] [ toInt(&current=&i) + 2 * toInt(&current=&j) ] |]

    onDomain (DomainSet _ _ (DomainReference n _)) | n == unnamedName = do
            (iPat , i ) <- quantifiedVar
            (jPat , j ) <- lettingVar
            (k1Pat, k1) <- quantifiedVar
            (k2Pat, k2) <- lettingVar
            let k2Val = swapIJ i j k1
            return $ Just [essence|
                and([ [ &k1 in &var          | &k1Pat : int(1..&unnamedSize) ]
                          >=lex
                      [ &k2 in &var          | &k1Pat : int(1..&unnamedSize)
                                             , letting &k2Pat be &k2Val
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
            let k2Val = swapIJ i j k1
            return $ Just [essence|
                and([ [ freq(&var, &k1)      | &k1Pat : int(1..&unnamedSize) ]
                          >=lex
                      [ freq(&var, &k2)      | &k1Pat : int(1..&unnamedSize)
                                             , letting &k2Pat be &k2Val
                                             ]
                    | &iPat : int(1..&unnamedSize - 1)
                    , letting &jPat be &i + 1
                    ])
                           |]

    -- this may be better for total functions, doesn't seem so for partial functions
    -- onDomain (DomainFunction _ _ (DomainReference n _) _) | n == unnamedName = do
    --         (iPat , i ) <- quantifiedVar
    --         (jPat , j ) <- lettingVar
    --         return $ Just [essence|
    --             and([ imageSet(&var, &i)
    --                       ~>=
    --                   imageSet(&var, &j)
    --                 | &iPat : int(1..&unnamedSize - 1)
    --                 , letting &jPat be &i + 1
    --                 ])
    --                        |]

    onDomain (DomainFunction _ _ (DomainReference n _) domTo) | n == unnamedName = do
            (iPat , i ) <- quantifiedVar
            (jPat , j ) <- lettingVar
            (k1Pat, k1) <- quantifiedVar
            (k2Pat, k2) <- lettingVar
            let k2Val = swapIJ i j k1
            (zPat , z ) <- quantifiedVar
            return $ Just [essence|
                and([ [ image(&var, &k1) = &z
                                             | &k1Pat : int(1..&unnamedSize)
                                             , &zPat  : &domTo
                                             ]
                          >=lex
                      [ image(&var, &k2) = &z
                                             | &k1Pat : int(1..&unnamedSize)
                                             , &zPat  : &domTo
                                             , letting &k2Pat be &k2Val
                                             ]
                    | &iPat : int(1..&unnamedSize - 1)
                    , letting &jPat be &i + 1
                    ])
                           |]

    -- this may be better for total functions, doesn't seem so for partial functions
    -- onDomain (DomainFunction _ _ _ (DomainReference n _)) | n == unnamedName = do
    --         (iPat , i ) <- quantifiedVar
    --         (jPat , j ) <- lettingVar
    --         return $ Just [essence|
    --             and([ preImage(&var, &i)
    --                       ~>=
    --                   preImage(&var, &j)
    --                 | &iPat : int(1..&unnamedSize - 1)
    --                 , letting &jPat be &i + 1
    --                 ])
    --                        |]

    onDomain (DomainFunction _ _ domFr (DomainReference n _)) | n == unnamedName = do
            (iPat , i ) <- quantifiedVar
            (jPat , j ) <- lettingVar
            (k1Pat, k1) <- quantifiedVar
            (k2Pat, k2) <- lettingVar
            let k2Val = swapIJ i j k1
            (zPat , z ) <- quantifiedVar
            return $ Just [essence|
                and([ [ image(&var, &z) = &k1
                                             | &zPat  : &domFr
                                             , &k1Pat : int(1..&unnamedSize)
                                             ]
                          >=lex
                      [ image(&var, &z) = &k2
                                             | &zPat  : &domFr
                                             , &k1Pat : int(1..&unnamedSize)
                                             , letting &k2Pat be &k2Val
                                             ]
                    | &iPat : int(1..&unnamedSize - 1)
                    , letting &jPat be &i + 1
                    ])
                           |]

    onDomain (DomainFunction _ _ (DomainTuple [DomainReference n _, domFr2]) domTo) | n == unnamedName = do
            (iPat , i ) <- quantifiedVar
            (jPat , j ) <- lettingVar
            (k1Pat, k1) <- quantifiedVar
            (k2Pat, k2) <- lettingVar
            let k2Val = swapIJ i j k1
            (z1Pat, z1) <- quantifiedVar
            (z2Pat, z2) <- quantifiedVar
            return $ Just [essence|
                and([ [ image(&var, (&k1, &z1)) = &z2
                                             | &k1Pat : int(1..&unnamedSize)
                                             , &z1Pat : &domFr2
                                             , &z2Pat : &domTo
                                             ]
                          >=lex
                      [ image(&var, (&k2, &z1)) = &z2
                                             | &k1Pat : int(1..&unnamedSize)
                                             , &z1Pat : &domFr2
                                             , &z2Pat : &domTo
                                             , letting &k2Pat be &k2Val
                                             ]
                    | &iPat : int(1..&unnamedSize - 1)
                    , letting &jPat be &i + 1
                    ])
                           |]

    onDomain (DomainFunction _ _ (DomainTuple [domFr1, DomainReference n _]) domTo) | n == unnamedName = do
            (iPat , i ) <- quantifiedVar
            (jPat , j ) <- lettingVar
            (k1Pat, k1) <- quantifiedVar
            (k2Pat, k2) <- lettingVar
            let k2Val = swapIJ i j k1
            (z1Pat, z1) <- quantifiedVar
            (z2Pat, z2) <- quantifiedVar
            return $ Just [essence|
                and([ [ image(&var, (&z1, &k1)) = &z2
                                             | &k1Pat : int(1..&unnamedSize)
                                             , &z1Pat : &domFr1
                                             , &z2Pat : &domTo
                                             ]
                          >=lex
                      [ image(&var, (&z1, &k2)) = &z2
                                             | &k1Pat : int(1..&unnamedSize)
                                             , &z1Pat : &domFr1
                                             , &z2Pat : &domTo
                                             , letting &k2Pat be &k2Val
                                             ]
                    | &iPat : int(1..&unnamedSize - 1)
                    , letting &jPat be &i + 1
                    ])
                           |]

    onDomain _ = return Nothing

