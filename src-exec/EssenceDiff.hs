{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import Control.Applicative
import Control.Monad.Error ( runErrorT )
import Control.Monad.Writer ( runWriter )
import Data.List ( isSuffixOf )
import System.Environment ( getArgs )
import qualified Data.Text.Lazy.IO as T

import Nested
import Language.Essence ( Spec )
import Language.Essence.Phases.ReadIn ( readIn )
import PrintUtils ( ($$), Doc )



main :: IO ()
main = do
    specFilenames <- filter (".essence" `isSuffixOf`) <$> getArgs
    case specFilenames of
        [aFilename,bFilename] -> do

            maSpec <- getSpec aFilename
            mbSpec <- getSpec bFilename

            case (maSpec,mbSpec) of
                (Right aSpec, Right bSpec) ->
                    if (aSpec :: Spec) == (bSpec :: Spec)
                        then putStrLn "Same."
                        else putStrLn "Not same."
                (Left aErr, Left bErr) -> error $ show (nestedToDoc aErr $$ nestedToDoc bErr)
                (Left aErr, _)         -> error $ show aErr
                (_, Left bErr)         -> error $ show bErr

        _      -> error "Expected two *.essence files."


getSpec :: FilePath -> IO (Either (Nested Doc) Spec)
getSpec filename = do
    contents <- T.readFile filename
    let (spec,logs) = runWriter $ runErrorT $ readIn (Just filename) contents
    mapM_ print logs
    return spec
