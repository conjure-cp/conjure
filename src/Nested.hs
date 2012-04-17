{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}

module Nested where

import Data.String ( IsString(..) )
import Control.Monad.Error ( Error(strMsg), MonadError(throwError,catchError) )

import qualified PrintUtils as P

data Nested a = Nested (Maybe a) [Nested a] deriving Show

instance Error (Nested P.Doc) where
    strMsg = fromString

addToTop :: a -> [Nested a] -> Nested a
addToTop a ts = Nested (Just a) ts

instance IsString a => IsString (Nested a) where
    fromString s = addToTop (fromString s) []

nestedToDoc :: Nested P.Doc -> P.Doc
nestedToDoc (Nested Nothing  xs) = P.vcat $ map nestedToDoc xs
nestedToDoc (Nested (Just x) xs) = P.hang x 4 (P.vcat $ map nestedToDoc xs)

singletonNested :: a -> Nested a
singletonNested = flip Nested [] . Just

throwErrorSingle :: (MonadError (Nested e) m) => e -> m a
throwErrorSingle = throwError . flip Nested [] . Just

infix 0 <?>
(<?>) :: (MonadError (Nested e) m) => m a -> e -> m a
comp <?> msg = comp `catchError` \ err -> throwError $ addToTop msg [err]

infix 0 <??>
(<??>) :: (MonadError (Nested e) m) => m a -> e -> m a
comp <??> msg = comp `catchError` \ _  -> throwError $ addToTop msg []
