module Conjure.Process.ValidateConstantForDomain ( validateConstantForDomain ) where

import Conjure.Prelude
import Conjure.Language
import Conjure.Process.Enumerate ( EnumerateDomain )

validateConstantForDomain ::
    forall m r .
    MonadFailDoc m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Pretty r =>
    Eq r =>
    Name -> Constant -> Domain r Constant -> m ()
