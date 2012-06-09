{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Properties.Simplify where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Properties.Pretty
import qualified Language.Core.Properties.Simplify.Internal as Internal ( simplify )


simplify :: (Functor m, Monad m) => Core -> WriterT Any (CompT m) Core
simplify x = do
    (x', Any flag) <- lift $ runWriterT (Internal.simplify x)
    if flag
        then do
            tell (Any True)
            lift $ mkLog "simplify-step" $ vcat [pretty x, "~~>", pretty x']
            simplify x'
        else return x
