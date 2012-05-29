-- things in Language.Core.Properties.* should import this.

module Language.Core ( module X ) where

import Language.Core.Imports    as X
import Language.Core.Definition as X

import Language.Core.MatchBind  as X
import Language.Core.MultiMatch as X
import Language.Core.Parser     as X

import Language.Core.Properties.CategoryOf  as X
import Language.Core.Properties.DomainOf    as X
import Language.Core.Properties.IsSafe      as X
-- import Language.Core.Properties.IsValid     as X
import Language.Core.Properties.MkSafe      as X
import Language.Core.Properties.ShowAST     as X
import Language.Core.Properties.Simplify    as X
import Language.Core.Properties.ToBool      as X
import Language.Core.Properties.ToInt       as X
import Language.Core.Properties.ToLit       as X
import Language.Core.Properties.TypeOf      as X
