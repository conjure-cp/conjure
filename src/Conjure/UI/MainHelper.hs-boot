module Conjure.UI.MainHelper ( mainWithArgs ) where

import Conjure.Prelude
import Conjure.UI ( UI )
import Conjure.Language.TypeOf ( TypeCheckerMode(..) )
import {-# SOURCE #-} Conjure.Process.Enumerate ( EnumerateDomain )


mainWithArgs ::
    MonadIO m =>
    MonadLog m =>
    MonadFail m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    UI -> m ()
