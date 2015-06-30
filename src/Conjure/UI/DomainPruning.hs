module Conjure.UI.DomainPruning where

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.NameResolution ( resolveNames )
import Conjure.Compute.DomainOf ( domainOf )


-- | At the moment: Just enumerate all subexpressions in the model together with their computed domains.
domainPruning :: (MonadLog m, MonadUserError m, MonadFail m, MonadIO m) => Model -> m ()
domainPruning = runNameGen . (resolveNames >=> core)
    where
        core :: (NameGen m, MonadFail m, MonadIO m) => Model -> m ()
        core m =
            forM_ (universeBi (mStatements m)) $ \ x -> do
                -- mdom <- domainOf x
                -- let domTxt = case mdom of
                --                 Left err  -> err
                --                 Right dom -> pretty dom
                dom <- domainOf x
                liftIO $ print $ vcat
                    [ "Term  :" <+> pretty (x :: Expression)
                    , "Domain:" <+> pretty dom
                    ]
