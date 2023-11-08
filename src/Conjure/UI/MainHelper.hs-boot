module Conjure.UI.MainHelper ( mainWithArgs ) where

import Conjure.Prelude
import Conjure.UI ( UI )
import Conjure.Language.Type ( TypeCheckerMode(..) )
import {-# SOURCE #-} Conjure.Process.Enumerate ( EnumerateDomain )
import Conjure.Language.NameGen ( NameGen )


mainWithArgs ::
    MonadIO m =>
    MonadLog m =>
    MonadFailDoc m =>
    EnumerateDomain m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    UI -> m ()
