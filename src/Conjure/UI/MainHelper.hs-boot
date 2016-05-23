module Conjure.UI.MainHelper ( mainWithArgs ) where

import Conjure.Prelude
import Conjure.UI ( UI )
import {-# SOURCE #-} Conjure.Process.Enumerate ( EnumerateDomain )


mainWithArgs :: (MonadIO m, MonadLog m, MonadFail m, EnumerateDomain m) => UI -> m ()
