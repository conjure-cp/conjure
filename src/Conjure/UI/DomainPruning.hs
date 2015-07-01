module Conjure.UI.DomainPruning where

import Conjure.Prelude
import Conjure.Language
import Conjure.UI.TypeCheck ( typeCheckModel_StandAlone )
import Conjure.Compute.DomainOf ( domainOf )


-- | At the moment: Just enumerate all subexpressions in the model together with their computed domains.
domainPruning :: (MonadUserError m, MonadLog m, MonadFail m) => Bool -> Model -> m ()
domainPruning showErrors = runNameGen . (typeCheckModel_StandAlone >=> core)
    where
        core :: (NameGen m, MonadLog m, MonadFail m) => Model -> m ()
        core m =
            forM_ (universeBi (mStatements m)) $ \ x -> do
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

