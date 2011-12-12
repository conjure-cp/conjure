{-# LANGUAGE TemplateHaskell #-}

module Constants where

import Data.ByteString.Char8 ( unpack)
import Data.FileEmbed ( embedFile )


figlet :: String
figlet = unpack $(embedFile "datafiles/conjure.figlet")

-- reservedNames are loaded from `reservedNames.txt` at compile time
reservedNamesTxt :: String
reservedNamesTxt = unpack $(embedFile "datafiles/reservedNames.txt")

-- reservedOpNames are loaded from `reservedOpNames.txt` at compile time
reservedOpNamesTxt :: String
reservedOpNamesTxt = unpack $(embedFile "datafiles/reservedOpNames.txt")
