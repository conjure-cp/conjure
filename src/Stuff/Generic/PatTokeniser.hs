{-# LANGUAGE FlexibleContexts #-}

module Stuff.Generic.PatTokeniser where

import Control.Applicative
import Data.Char
import Text.Parsec ( ParsecT, Stream, tokenPrim, incSourceColumn, (<?>) )

data PatToken
    = PatOpenBracket
    | PatCloseBracket
    | PatOpenBrace
    | PatCloseBrace
    | PatComma
    | PatColon
    | PatIdentifier String
    | PatMeta       String
    deriving (Eq,Show)


tokenise :: String -> Either String [PatToken]
tokenise []       = Right []
tokenise ('[':xs) = (PatOpenBracket :) <$> tokenise xs
tokenise (']':xs) = (PatCloseBracket:) <$> tokenise xs
tokenise ('{':xs) = (PatOpenBrace   :) <$> tokenise xs
tokenise ('}':xs) = (PatCloseBrace  :) <$> tokenise xs
tokenise (',':xs) = (PatComma       :) <$> tokenise xs
tokenise (':':xs) = (PatColon       :) <$> tokenise xs
tokenise ('&':x:xs) | isLetter x = (PatMeta       (x:ys) :) <$> tokenise rest
    where (ys,rest) = span isAlphaNum xs
tokenise (    x:xs) | isLetter x = (PatIdentifier (x:ys) :) <$> tokenise rest
    where (ys,rest) = span isAlphaNum xs
tokenise (' ':xs) = tokenise xs
tokenise xs = Left ("tokeniser: " ++ xs)

satisfyT :: Stream s m PatToken => (PatToken -> Bool) -> ParsecT s u m PatToken
satisfyT predicate = tokenPrim showTok nextPos testTok
    where
      showTok tok       = show tok
      testTok tok       = if predicate tok then Just tok else Nothing
      nextPos pos tok _ = incSourceColumn pos $ case tok of
                                                    PatIdentifier s -> length s
                                                    PatMeta       s -> length s + 1
                                                    _               -> 1

token :: Stream s m PatToken => ParsecT s u m PatToken
token = satisfyT (const True)

pIdentifier :: Stream s m PatToken => ParsecT s u m String
pIdentifier = do PatIdentifier s <- satisfyT f; return s <?> "identifier"
    where f PatIdentifier {} = True
          f _                = False

pMeta :: Stream s m PatToken => ParsecT s u m String
pMeta = do PatMeta s <- satisfyT f; return s <?> "meta variable"
    where f PatMeta {} = True
          f _          = False
