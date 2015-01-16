module Conjure.Process.LettingsForComplexInDoms
    ( lettingsForComplexInDoms
    ) where

import Conjure.Prelude
import Conjure.Language.Definition


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
            nms <- gets id
            modify $ const $ tail nms
            return (head nms)

        expressionExtract expr@Constant{}        = return expr
        expressionExtract expr@AbstractLiteral{} = return expr
        expressionExtract expr@Reference{}       = return expr
        expressionExtract expr = do
            newLetting <- nextName
            tell [Declaration (Letting newLetting expr)]        -- new declarations
            return (Reference newLetting Nothing)               -- the replacement expression

    flip evalStateT (freshNames m) $ do
        statements <- forM (mStatements m) $ \ st ->
            case st of
                Declaration (FindOrGiven forg name domain) -> do
                    (domain', newLettings) <- runWriterT (mapM expressionExtract domain)
                    return (newLettings ++ [Declaration (FindOrGiven forg name domain')])
                Declaration (LettingDomainDefnUnnamed name expr) -> do
                    (expr', newLettings) <- runWriterT (expressionExtract expr)
                    return (newLettings ++ [Declaration (LettingDomainDefnUnnamed name expr')])                    
                _ -> return [st]
        return m { mStatements = concat statements }
