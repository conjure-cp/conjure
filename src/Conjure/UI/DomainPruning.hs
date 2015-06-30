module Conjure.UI.DomainPruning where

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.NameResolution ( resolveNames )
import Conjure.Compute.DomainOf ( domainOf )


-- | At the moment: Just enumerate all subexpressions in the model together with their computed domains.
domainPruning :: (MonadUserError m, MonadLog m, MonadFail m) => Model -> m ()
domainPruning = runNameGen . (resolveNames >=> core)
    where
        core :: (NameGen m, MonadLog m, MonadFail m) => Model -> m ()
        core m =
            forM_ (universeBi (mStatements m)) $ \ x -> do
                mdom <- runExceptT $ domainOf x
                logInfo $ vcat
                    [ "Term  :" <+> pretty (x :: Expression)
                    , case mdom of
                        Left  err -> "Error :" <+> err
                        Right dom -> "Domain:" <+> pretty dom
                    , ""
                    ]
