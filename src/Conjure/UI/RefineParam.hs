
-- Given
--     * An Essence spec
--     * An Essence' model for the spec
--     * An EssenceParam file
-- Generate
--     * An Essence'Param file

-- This module is named after the diagram we drew in IanM's room. It
-- probably shouldn't be.

{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Conjure.UI.RefineParam ( refineParam ) where

-- conjure
import Conjure.Language.Definition
import Conjure.Language.Instantiate
import Conjure.Representations ( down_ )
import Bug
import Language.E


refineParam
    :: ( Functor m
       , Applicative m
       , MonadError Doc m
       )
    => Model     -- eprime model
    -> Model     -- essence param
    -> m Model    -- eprime param
refineParam eprimeModel essenceParam = do

    let essenceLettings = extractLettings essenceParam
    let essenceGivens = eprimeModel |> mInfo |> miRepresentations

    -- TODO: check if for every given there is a letting (there can be more)
    -- TODO: check if the same letting has multiple values for it

    essenceLettings' <- forM essenceLettings $ \ (name, val) -> do
        constant <- instantiateExpression essenceLettings val
        return (name, constant)

    essenceGivens' <- forM essenceGivens $ \ (name, dom) -> do
        constant <- instantiateDomain essenceLettings dom
        return (name, constant)

    let essenceGivensAndLettings =
            [ case lookup n essenceLettings' of
                Nothing -> userErr $ vcat
                            [ "No value for parameter:" <+> pretty n
                            , "With domain:" <+> pretty d
                            ]
                Just v  -> (n, d, v)
            | (n, d) <- essenceGivens'
            ]

    eprimeLettings <- liftM concat $ mapM down_ essenceGivensAndLettings

    return $ Model [ Declaration (Letting n (C x))
                   | (n, _, x) <- eprimeLettings
                   ]
                   def

extractLettings :: Model -> [(Name, Expression)]
extractLettings model =
    [ (n, x) | Declaration (Letting n x) <- mStatements model ]

