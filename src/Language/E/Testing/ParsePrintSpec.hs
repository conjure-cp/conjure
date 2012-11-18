{-# LANGUAGE OverloadedStrings #-}

module Language.E.Testing.ParsePrintSpec where

import Language.E

import System.Directory ( getDirectoryContents )

import Test.Hspec
import Test.Hspec.HUnit ()
import Test.HUnit

import qualified Data.Text as T
import qualified Data.Text.IO as T


getSpecs :: FilePath -> IO [FilePath]
getSpecs fp = do
    xs <- getDirectoryContents fp
    let ys = filter (".essence" `isSuffixOf`) xs
    return [ fp ++ "/"  ++ f | f <- ys ]

specs :: IO [FilePath]
specs = allFilesWithSuffix ".essence" "files/testdata"

tests :: IO Test.Hspec.Spec
tests = do
    xs <- specs
    return $
        describe "parsing and pretty printing" $
            forM_ xs $ \ filepath ->
                it filepath $ one filepath

one :: FilePath -> IO ()
one filepath = do
    content <- T.readFile filepath
    result  <- runErrorT $ do
        -- liftIO $ putStrLn "content"
        -- liftIO $ putStrLn $ T.unpack content
        parsed1 <- runLexerAndParser parseSpec filepath content
        let printed = T.pack $ show $ pretty parsed1
        -- liftIO $ putStrLn "parse 1"
        -- liftIO $ print $ pretty parsed1
        -- liftIO $ T.writeFile "last.essence" $ printed
        parsed2 <- runLexerAndParser parseSpec "<memory>" printed
        -- liftIO $ putStrLn "parse 2"
        -- liftIO $ print $ pretty parsed2
        -- liftIO $ writeFile "last2.essence" $ show $ pretty parsed2
        return (parsed1, parsed2)

    case result of
        Left  e        -> assertFailure $ show e
        Right (p1@(Spec _ s1), p2@(Spec _ s2)) -> do
            print $ pretty p1
            print $ pretty p2
            unless (p1 == p2) $
                -- assertFailure $ show $ vcat [ "==> Expecting:", pretty p1, "==> But got:", pretty p2]
                -- assertFailure $ show $ vcat [ "==> Expecting:", prettyAsPaths p1, "==> But got:", prettyAsPaths p2]
                let out = vcat $ concat $ take 1 [ [prettyAsPaths x, "", prettyAsPaths y]
                                                 | (x,y) <- zip (statementAsList s1) (statementAsList s2)
                                                 , x /= y
                                                 ]
                in assertFailure $ show out

