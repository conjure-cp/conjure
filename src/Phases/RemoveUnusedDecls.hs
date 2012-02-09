{-# LANGUAGE FlexibleContexts #-}

module Phases.RemoveUnusedDecls where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO )
import Data.Generics.Uniplate.Direct ( Biplate, universeBi )
import Data.List ( (\\) )
import Data.List.Split ( splitOn )

import Language.Essence
import Utils


removeUnusedDecls :: (Applicative m, MonadIO m) => Spec -> m Spec
removeUnusedDecls spec
    = return $ spec { topLevelBindings = [ b
                                         | b@(_,bName,_) <- topLevelBindings spec
                                         , isReferenced bName (objective spec)
                                           ||
                                           isReferenced bName (constraints spec)
                                           ||
                                           isReferenced bName (map thd3 ((topLevelBindings spec) \\ [b]))
                                         ] }

isReferenced :: Biplate a Expr => String -> a -> Bool
isReferenced nm x = not $ null [ ()
                               | Identifier nm' <- universeBi x
                               , nm == head (splitOn "#" nm')
                               ]
