{-# LANGUAGE CPP #-}
{-# LANGUAGE TemplateHaskell #-}

#define DATADIR "datafiles/"

module Constants where

import Data.ByteString.Char8 ( unpack)
import Data.FileEmbed ( embedFile )


figlet :: String
figlet = unpack $(embedFile (DATADIR ++ "conjure.figlet"))

-- reservedNames are loaded from `reservedNames.txt` at compile time
reservedNamesTxt :: String
reservedNamesTxt = unpack $(embedFile (DATADIR ++ "reservedNames.txt"))

-- reservedOpNames are loaded from `reservedOpNames.txt` at compile time
reservedOpNamesTxt :: String
reservedOpNamesTxt = unpack $(embedFile (DATADIR ++ "reservedOpNames.txt"))
