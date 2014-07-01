{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.TranslateSolution ( translateSingleSolution, translateSolution ) where

-- conjure
import Conjure.UpDown
import Language.E.Imports
import Language.E.Definition


-- | Translating a collection of low level constants to a high level constant.
--   The high level domain (with representations) is taken as an argument.
translateSingleSolution
    :: MonadError UpDownError m
    => Text
    -> Domain Representation Constant
    ->  [(Text, Constant)]
    -> m (Text, Constant)
translateSingleSolution _highName _highDomain _lows = error "translateSingleSolution"


translateSolution
    :: MonadError UpDownError m
    => Spec                         -- ^ Essence
    -> Spec                         -- ^ Essence'
    -> Spec                         -- ^ Essence' Solution
    -> m Spec                       -- ^ Essence Solution
translateSolution = error "translateSolution"

