{-# LANGUAGE FlexibleContexts #-}

module Conjure.UI.TranslateSolution ( translateSolution ) where

-- conjure
import Conjure.Language.Definition
import Conjure.Language.Instantiate
import Conjure.Representations ( up )
import Language.E.Imports


translateSolution
    :: ( Functor m
       , Applicative m
       , MonadError Doc m
       )
    => Model      -- eprime model
    -> Model      -- essence param
    -> Model      -- eprime solution
    -> m Model    -- essence solution
translateSolution eprimeModel essenceParam eprimeSolution = do

    let eprimeLettings = extractLettings essenceParam ++
                         extractLettings eprimeSolution
    let essenceGivens = eprimeModel |> mInfo |> miRepresentations

    eprimeLettings' <- forM eprimeLettings $ \ (name, val) -> do
        constant <- instantiateExpression eprimeLettings val
        return (name, constant)

    essenceGivens' <- forM essenceGivens $ \ (name, dom) -> do
        constant <- instantiateDomain eprimeLettings dom
        return (name, constant)

    essenceLettings <- mapM (up eprimeLettings') essenceGivens'

    return $ Model [ Declaration (Letting n (Constant x))
                   | (n, x) <- essenceLettings
                   ]
                   def

extractLettings :: Model -> [(Name, Expression)]
extractLettings model =
    [ (n, x) | Declaration (Letting n x) <- mStatements model ]

