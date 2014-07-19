{-# LANGUAGE OverloadedStrings #-}
{-# OPTIONS_GHC -fno-warn-orphans #-}

module Conjure.Language.Pretty
    ( module Language.E.Pretty
    ) where

import Conjure.Language.Definition
import Bug
import Language.E.Pretty


instance Pretty Model where
    pretty = bug "pretty Model"
