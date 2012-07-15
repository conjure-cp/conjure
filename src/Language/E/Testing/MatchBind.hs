{-# LANGUAGE OverloadedStrings #-}

module Language.E.Testing.MatchBind where

import Stuff.CompT ( runCompT )
import Stuff.Generic ( prettyAsPaths )
import Stuff.Pretty ( pretty )
import Language.E

import Control.Applicative ( (<$>) )
import Control.Monad ( (>=>), forM_, unless )
import Control.Monad.Trans.Maybe ( runMaybeT )
import Data.List ( isSuffixOf )
import Data.Default ( def )
import System.Directory ( getDirectoryContents )
import Text.PrettyPrint ( vcat )

import Test.Hspec.Monadic
import Test.Hspec.HUnit ()
import Test.HUnit

import qualified Data.Text as T
import qualified Data.Text.IO as T


shouldMatch :: T.Text -> T.Text -> Test.Hspec.Monadic.Spec
shouldMatch patternText actualText
    = it (concat [ "should    match: "
                 , T.unpack patternText
                 , " ~~ "
                 , T.unpack actualText
                 ]) $ tryMatch patternText actualText

tryMatch :: T.Text -> T.Text -> IO ()
tryMatch patternText actualText = do
    flag <- runCompT def def $ do
        pattern <- runLexerAndParser parseExpr "" patternText
        actual  <- runLexerAndParser parseExpr "" actualText
        patternMatch pattern actual
    case flag of
        [(Right True,_,logs)] -> print $ prettyLogs logs
        -- _ -> fail $ show flag
        _ -> fail "expected to match, but didn't."

shouldn'tMatch :: T.Text -> T.Text -> Test.Hspec.Monadic.Spec
shouldn'tMatch patternText actualText
    = it (concat [ "shouldn't match: "
                 , T.unpack patternText
                 , " ~~ "
                 , T.unpack actualText
                 ]) $ tryNoMatch patternText actualText

tryNoMatch :: T.Text -> T.Text -> IO ()
tryNoMatch patternText actualText = do
    flag <- runCompT def def $ do
        pattern <- runLexerAndParser parseExpr "" patternText
        actual  <- runLexerAndParser parseExpr "" actualText
        patternMatch pattern actual
    case flag of
        [(Right False,_,logs)] -> print $ prettyLogs logs
        -- _ -> fail $ show flag
        _ -> fail "expected not to match, but did."

shouldBindTo :: T.Text -> T.Text -> [(String, T.Text)] -> Test.Hspec.Monadic.Spec
shouldBindTo patternText expectedText bs
    = it (concat [ "should bind to: "
                 , T.unpack patternText
                 , " ~~ "
                 , T.unpack expectedText
                 ]) $ tryBind patternText expectedText bs

tryBind :: T.Text -> T.Text -> [(String,T.Text)] -> IO ()
tryBind patternText expectedText bs = do
    out <- runCompT def def $ do
        forM_ bs $ \ (nm,xText) -> do
            x <- runLexerAndParser parseExpr "" xText
            addBinder nm x
        pattern  <- runLexerAndParser parseExpr "" patternText
        expected <- runLexerAndParser parseExpr "" expectedText
        mbound   <- runMaybeT $ patternBind pattern
        case mbound of
            Nothing    -> fail "There are unbound things."
            Just bound ->
                if expected == bound
                    then return ()
                    else fail "Expected /= Bound"
    case out of
        [(Right (), _, logs)] -> print $ prettyLogs logs
        _ -> fail "Unknown error in tryBind"

tests :: Test.Hspec.Monadic.Spec
tests = describe "pattern matching" $ do
    shouldMatch         "@x + @y"
                        "1 + 2"
    shouldn'tMatch      "@x + @y"
                        "1 - 2"
    shouldMatch         "`set (size 2) of int`"
                        "`set (size 2) of int`"
    shouldn'tMatch      "`set (size 3) of int`"
                        "`set (size 2) of int`"
    shouldn'tMatch      "`set (size 2) of int`"
                        "`set (size 3) of int`"
    shouldMatch         "`set (size @a) of int`"
                        "`set (size 2) of int`"
    shouldn'tMatch      "`set (size @a) of int`"
                        "`set (size 2, minSize 3) of int`"
    shouldMatch         "`set (size @a,..) of int`"
                        "`set (size 2, minSize 3) of int`"
    shouldMatch         "`set (size @a, minSize @b) of int`"
                        "`set (size 2, minSize 3) of int`"
    shouldMatch         "`set (minSize @a,size @b) of int`"
                        "`set (size 2, minSize 3) of int`"
    shouldMatch         "`set (size @a, minSize @b,..) of int`"
                        "`set (size 2, minSize 3) of int`"
    shouldMatch         "`set (minSize @a, size @b,..) of int`"
                        "`set (size 2, minSize 3) of int`"
    shouldn'tMatch      "`set (size @a, minSize @b, maxSize @c) of int`"
                        "`set (size 2, minSize 3) of int`"
    shouldn'tMatch      "`set (minSize @a, size @b, maxSize @c) of int`"
                        "`set (size 2, minSize 3) of int`"
    shouldBindTo        "@a + @b"
                        "2 + 3"
                        [ ("@a", "2")
                        , ("@b", "3")
                        ]
    shouldBindTo        "@a + @b"
                        "2 + 3"
                        [ ("@b", "3")
                        , ("@a", "2")
                        ]
    shouldBindTo        "@a + @b"
                        "2 + 3"
                        [ ("@a", "2")
                        , ("@b", "3")
                        , ("@c", "4")
                        ]
    shouldBindTo        "@a + @b"
                        "2 + 3"
                        [ ("@c", "4")
                        , ("@a", "2")
                        , ("@b", "3")
                        ]
