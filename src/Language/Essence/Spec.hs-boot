{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Language.Essence.Spec where

import {-# SOURCE #-} Language.Essence.Binding
import Language.Essence.Where
import {-# SOURCE #-} Language.Essence.Objective
import {-# SOURCE #-} Language.Essence.Expr
import Language.Essence.Metadata


data Spec

    = Spec { language    :: String
           , version     :: [Int]
           , topLevels   :: [Either Binding Where]
           , objective   :: Maybe Objective
           , constraints :: [Expr]
           , metadata    :: [Metadata]
           }
