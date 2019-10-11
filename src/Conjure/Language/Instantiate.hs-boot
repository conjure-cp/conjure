module Conjure.Language.Instantiate ( instantiateExpression ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Type
import Conjure.Process.Enumerate ( EnumerateDomain )


instantiateExpression ::
    MonadFail m =>
    EnumerateDomain m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    [(Name, Expression)] -> Expression -> m Constant
