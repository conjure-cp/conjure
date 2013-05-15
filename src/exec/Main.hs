module Main where

import Conjure ( getConjureMode, runConjureMode,conjureHelp )

main :: IO ()
main = do
    maybeMode <- getConjureMode
    case maybeMode of
        Nothing -> error $  "Cannot parse command line arguments.\n" ++ show conjureHelp
        Just mode -> runConjureMode mode

