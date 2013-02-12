module Main where

import Conjure ( getConjureMode, runConjureMode )


main :: IO ()
main = do
    maybeMode <- getConjureMode
    case maybeMode of
        Nothing -> error "Cannot parse command line arguments."
        Just mode -> runConjureMode mode

