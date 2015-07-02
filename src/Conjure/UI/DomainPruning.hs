module Conjure.UI.DomainPruning where

import Conjure.Prelude
import Conjure.Language
import Conjure.UI.TypeCheck ( typeCheckModel_StandAlone )
import Conjure.Compute.DomainOf ( domainOf )


-- | At the moment: Just enumerate all subexpressions in the model together with their computed domains.
domainPruning :: (MonadUserError m, MonadLog m, MonadFail m) => Bool -> [String] -> [String] -> Model -> m ()
domainPruning showErrors only includes = runNameGen . flip evalStateT def . (typeCheckModel_StandAlone >=> core)
    where
        core :: (NameGen m, MonadLog m, MonadFail m, MonadState [String] m) => Model -> m ()
        core m =
            forM_ (universeBi (mStatements m)) $ \ x -> do
                let xPretty = renderWide (pretty x)
                seenBefore <- gets (xPretty `elem`)
                when (not seenBefore) $
                    when ( (null only && null includes)
                           || any (==          xPretty) only
                           || any (`isInfixOf` xPretty) includes) $ do
                        modify (xPretty :)
                        mdom <- runExceptT $ domainOf (x :: Expression)
                        case mdom of
                            Left err | showErrors -> logInfo $ vcat
                                [ "Term  :" <+> pretty x
                                , "Error :" <+> err
                                , ""
                                ]
                            Right dom | not showErrors -> logInfo $ vcat
                                [ "Term  :" <+> pretty x
                                , "Domain:" <+> pretty dom
                                , ""
                                ]
                            _ -> return ()
