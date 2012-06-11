{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ViewPatterns #-}

module Language.Core.Properties.CategoryOf where

import Language.Core.Imports
import Language.Core.Definition
import Language.Core.Parser


class CategoryOf a where
    categoryOf :: (Functor m, Monad m) => a -> CompT m [Category]

instance CategoryOf Core where
    categoryOf (L {}) = return [Constant]
    categoryOf (R r ) = categoryOf r
    categoryOf (Expr ":withlocals" [Expr ":actual" [a], Expr ":locals" bs]) = localState $ do
        mapM_ processStatement bs
        categoryOf a
    categoryOf (Expr ":find"  _) = return [Decision]
    categoryOf (Expr ":given" _) = return [Parameter]
    categoryOf (view -> ([":value",":value-literal"],[L _])) = return [Constant]
    categoryOf (Expr _ xs) = do
        concat <$> mapM categoryOf xs

instance CategoryOf Reference where
    categoryOf r = do
        val <- lookUpRef r
        categoryOf val


tests :: [([Category], Text)]
tests =
    [ ( [Constant]
      , "(a @ letting a be 1)"
      )
    , ( [Constant]
      , "1"
      )
    , ( [Decision]
      , "(a+b @ find a,b : int(0..9))"
      )
    , ( [Constant,Decision]
      , "(a+b+1 @ find a,b : int(0..9))"
      )
    , ( [Parameter]
      , "(a+b @ given a,b : int(0..9))"
      )
    , ( [Parameter,Decision]
      , "(a+b @ given a : int(0..9) find b : bool)"
      )
    , ( [Constant,Parameter,Decision]
      , "(a+b-1 @ given a : int(0..9) find b : bool)"
      )
    ]

testRunner1 :: ([Category], Text) -> Bool
testRunner1 (a,b) =
    let comp = do i <- runP Nothing parseExpr b; categoryOf i
    in  case runComp def def comp of
            [(Right a', _, _)] | a `setEq` a' -> True
            other -> error $ ppShow other

testRunner :: Bool
testRunner = all testRunner1 tests
