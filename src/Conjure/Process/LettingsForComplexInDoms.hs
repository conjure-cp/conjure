module Conjure.Process.LettingsForComplexInDoms
    ( lettingsForComplexInDoms
    ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain


-- | if the domain of a declaration contains a reference to another declaration (a given)
--   that needs a representation, hell breaks loose.
--   specifically, name-resolution needs to be rerun after representation selection.
--   however, this transformation (as part of `Conjure.UI.Model.prologue`) cleans this up by introducing
--   extra letting statements.
lettingsForComplexInDoms
    :: (MonadFail m, MonadLog m)
    => Model
    -> m Model
lettingsForComplexInDoms m = do
    let
        nextName = do
            (nms, prevs) <- gets id
            modify $ const $ (tail nms, prevs)
            return (head nms)

        expressionExtract expr = do
            previousDecls <- gets snd
            case [ () | Reference nm _ <- universeBi expr
                      , nm `elem` previousDecls
                      ] of
                [] ->    -- not seen previously
                    return expr
                _  -> do -- seen previously
                    newLetting <- nextName
                    tell [Declaration (Letting newLetting expr)]        -- new declarations
                    return (Reference newLetting Nothing)               -- the replacement expression

    flip evalStateT (freshNames m, []) $ do
        statements <- forM (mStatements m) $ \ st ->
            case st of
                Declaration (FindOrGiven forg name domain) -> do
                    (domain', newLettings) <- runWriterT (mapM expressionExtract domain)
                    unless (isPrimitiveDomain domain) $
                        modify $ \ (nms, prevs) -> (nms, name:prevs)    -- record this as a declaration
                    return (newLettings ++ [Declaration (FindOrGiven forg name domain')])
                _ -> return [st]
        return m { mStatements = concat statements }
