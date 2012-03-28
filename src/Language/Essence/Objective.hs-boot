{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Objective where

import GenericOps.Core
import ParsePrint



data Objective

--    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Objective

instance Hole Objective

instance GPlate Objective

instance MatchBind Objective

instance ParsePrint Objective
