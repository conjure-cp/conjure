{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

#define DATADIR "datafiles/"

module Constants ( figlet, reservedNamesTxt, reservedOpNamesTxt
                 , newRuleVar, isFreshName, freshNames
                 ) where

import Data.ByteString.Char8 ( unpack)
import Data.FileEmbed ( embedFile )
import Data.List ( isPrefixOf )
import Data.List.Split ( splitOn )
import Data.Set as S ( Set, fromList )

import Utils ( strip )



figlet :: String
figlet = unpack $(embedFile (DATADIR ++ "conjure.figlet"))

reservedSet :: S.Set String
reservedSet = S.fromList $ reservedNamesTxt ++ reservedOpNamesTxt

-- reservedNames are loaded from `reservedNames.txt` at compile time
reservedNamesTxt :: [String]
reservedNamesTxt
    = filter (not . null)
    $ map (strip . removeComment)
    $ lines
    $ unpack $(embedFile (DATADIR ++ "reservedNames.txt"))

-- reservedOpNames are loaded from `reservedOpNames.txt` at compile time
reservedOpNamesTxt :: [String]
reservedOpNamesTxt
    = filter (not . null)
    $ map (strip . removeComment)
    $ lines
    $ unpack $(embedFile (DATADIR ++ "reservedOpNames.txt"))

removeComment :: String -> String
removeComment s = case splitOn "#" s of (i:_) -> i

newRuleVar :: String -> String
newRuleVar = (ruleVarPrefix ++)

ruleVarPrefix :: String
ruleVarPrefix = "__INRULE_"

freshNames :: [String]
freshNames = [ "__" ++ show i | i <- [ (1 :: Integer) .. ] ]

isFreshName :: String -> Bool
isFreshName s = not (ruleVarPrefix `isPrefixOf` s) && "__" `isPrefixOf` s
