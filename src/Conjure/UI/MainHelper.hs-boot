module Conjure.UI.MainHelper ( mainWithArgs ) where

import Conjure.Prelude
import Conjure.UI ( UI )
import Conjure.Language.Type ( TypeCheckerMode(..) )
import {-# SOURCE #-} Conjure.Process.Enumerate ( EnumerateDomain )


mainWithArgs ::
    MonadIO m =>
    MonadLog m =>
    MonadFail m =>
    MonadFailDoc m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    UI -> m ()
