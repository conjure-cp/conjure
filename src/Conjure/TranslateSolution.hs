{-# LANGUAGE FlexibleContexts #-}

module Conjure.TranslateSolution ( translateSolution ) where

import Conjure.UpDown
import Language.E.Imports
import Language.E.Definition


translateSolution
    :: MonadError UpDownError m
    => Spec                         -- ^ Essence
    -> Spec                         -- ^ Essence'
    -> Spec                         -- ^ Essence' Solution
    -> m Spec                       -- ^ Essence Solution
translateSolution = error "translateSolution"

