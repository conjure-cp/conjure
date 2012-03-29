{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

#define DATADIR "datafiles/"

module Constants ( figlet, reservedNamesTxt, reservedOpNamesTxt
                 , newRuleVar, isFreshName, freshNames
                 ) where

import Data.ByteString.Char8 ( unpack)
import Data.FileEmbed ( embedFile )
import Data.List ( isPrefixOf )
import Data.Set as S ( Set, fromList )



figlet :: String
figlet = unpack $(embedFile (DATADIR ++ "conjure.figlet"))

reservedSet :: S.Set String
reservedSet = S.fromList $ reservedNamesTxt ++ reservedOpNamesTxt

-- reservedNames are loaded from `reservedNames.txt` at compile time
reservedNamesTxt :: [String]
reservedNamesTxt = lines $ unpack $(embedFile (DATADIR ++ "reservedNames.txt"))

-- reservedOpNames are loaded from `reservedOpNames.txt` at compile time
reservedOpNamesTxt :: [String]
reservedOpNamesTxt = lines $ unpack $(embedFile (DATADIR ++ "reservedOpNames.txt"))

newRuleVar :: String -> String
newRuleVar = (ruleVarPrefix ++)

ruleVarPrefix :: String
ruleVarPrefix = "__INRULE_"

freshNames :: [String]
freshNames = [ "__" ++ show i | i <- [ (1 :: Integer) .. ] ]

isFreshName :: String -> Bool
isFreshName s = not (ruleVarPrefix `isPrefixOf` s) && "__" `isPrefixOf` s
