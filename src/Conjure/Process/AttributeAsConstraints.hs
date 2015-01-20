module Conjure.Process.AttributeAsConstraints
    ( attributeAsConstraints
    ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Ops
import Conjure.Language.Pretty


-- | From the top level constraints, find the AACs and lift them to the domains of the declarations.
--   Complain for any remaining AACs.
attributeAsConstraints :: MonadFail m => Model -> m Model
attributeAsConstraints m = do

    (statements1, topLevelAACs) <- runWriterT $ forM (mStatements m) $ \ st -> case st of
        Where xs -> do
            xs1 <- liftM concat $ forM xs $ \ x -> case x of
                Op (MkOpAAC (OpAttributeAsConstraint (Reference nm _) attr val)) -> do
                    tell [(nm, attr, val)]
                    return []
                _ -> return [x]
            return [ Where xs1 | not (null xs1) ]
        SuchThat xs -> do
            xs1 <- liftM concat $ forM xs $ \ x -> case x of
                Op (MkOpAAC (OpAttributeAsConstraint (Reference nm _) attr val)) -> do
                    tell [(nm, attr, val)]
                    return []
                _ -> return [x]
            return [ SuchThat xs1 | not (null xs1) ]
        _ -> return [st]

    statements2 <- forM (concat statements1) $ \ st -> case st of
        Declaration (FindOrGiven forg name domain) -> do
            let newAttrs = [ (attr, val) | (nm, attr, val) <- topLevelAACs, name == nm ]
            domain' <- updateAttributes domain newAttrs
            return (Declaration (FindOrGiven forg name domain'))
        _ -> return st

    let
        check :: MonadFail m => Expression -> m ()
        check p@(Op (MkOpAAC (OpAttributeAsConstraint thing _ _))) =
            fail $ vcat [ "Cannot add an attribute to this:" <+> pretty thing
                        , "If you think this should be supported, please get in touch."
                        , "The relevant expression:" <+> pretty p
                        ]
        check _ = return ()

    -- perform the check
    mapM_ check (universeBi statements2)

    return m { mStatements = statements2 }
