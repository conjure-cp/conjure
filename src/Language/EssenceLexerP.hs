{-# LANGUAGE CPP #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoMonomorphismRestriction #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ViewPatterns #-}

module Language.EssenceLexerP where

import Control.Applicative
import Control.Monad
import Control.Monad.Error ( MonadError, throwError )
import Data.Either
import qualified Data.Text as T

import Nested ( Nested(..), addToTop, nestedToDoc )
import Language.EssenceLexer ( Lexeme(..), runLexer, lexemeFace, lexemeWidth )
import PrintUtils ( Doc, (<>), (<+>), text, renderDoc )
import qualified PrintUtils as P



type Stream = [Lexeme]



data Pos = Pos (Maybe FilePath) Integer Integer deriving Show

showPos :: Pos -> Doc
showPos (Pos  Nothing  line col) = "(at line" <+> P.integer line <+>
                                    "column"  <+> P.integer col  <> ")"
showPos (Pos (Just fp) line col) = "(in file" <+> text fp        <+>
                                    "at line" <+> P.integer line <+>
                                    "column"  <+> P.integer col  <> ")"



newtype Parser a = Parser { runParser :: Pos -> Stream -> Either (Nested Doc) [(a, Pos, Stream)] }

failWithMsg :: Doc -> Parser a
failWithMsg msg = Parser $ \ pos _ -> Left (addToTop (showPos pos <+> msg) [])

failUnexpected :: Lexeme -> Doc -> Parser a
failUnexpected l msg = failWithMsg ("parsing error in" <+> lexemeFace l <> ", expecting" <+> msg <> ".")

instance Functor Parser where
    fmap f parser = Parser $ \ pos stream ->
        case runParser parser pos stream of
            Left  msg     -> Left msg
            Right results -> Right [ (f a, p, s) | (a, p, s) <- results ]

instance Applicative Parser where
    pure  = return
    (<*>) = ap

instance Alternative Parser where
    empty = mzero
    (<|>) = mplus
    some p = (:) <$> p <*> many p
    many p = some p <||> return []

instance Monad Parser where
    fail msg = failWithMsg (text msg)
    return a = Parser $ \ pos stream -> Right [(a, pos, stream)]
    parser >>= f = Parser $ \ pos stream ->
        case runParser parser pos stream of
            Left  msg      -> Left msg
            Right results' ->
                let
                    applied = [ runParser (f a) p s | (a, p, s) <- results' ]
                    msgs    = lefts  applied
                    results = rights applied
                in  case results of
                        [] -> Left $ Nested Nothing msgs
                        _  -> Right (concat results)

instance MonadPlus Parser where
    mzero = fail "mzero"
    mplus a b = Parser $ \ pos stream ->
        let
            applied = [ runParser a pos stream, runParser b pos stream ]
            msgs    = lefts  applied
            results = rights applied
        in  case results of
                [] -> Left $ Nested Nothing msgs
                _  -> Right (concat results)



infix 0 <?>
(<?>) :: Parser a -> Doc -> Parser a
-- parser <?> msg = trace (show msg) $ Parser $ \ pos stream ->
parser <?> msg = Parser $ \ pos stream ->
    case runParser parser pos stream of
        Left  err     -> Left $ addToTop (showPos pos <+> msg) [err]
        Right results -> Right results

infix 0 <??>
(<??>) :: Parser a -> Doc -> Parser a
-- parser <??> msg = trace (show msg) $ Parser $ \ pos stream ->
parser <??> msg = Parser $ \ pos stream ->
    case runParser parser pos stream of
        Left  _       -> Left $ addToTop (showPos pos <+> msg) []
        Right results -> Right results

infixl 3 <||>
(<||>) :: Parser a -> Parser a -> Parser a
a <||> b = Parser $ \ pos stream ->
    case runParser a pos stream of
        Left  err1    -> case runParser b pos stream of
            Left  err2    -> Left $ Nested Nothing [err1, err2]
            Right results -> Right results
        Right results -> Right results


msum1 :: [Parser a] -> Parser a
msum1 = foldr1 (<||>)

next :: Parser Lexeme
next = satisfy (const True) <?> "next"

putBack :: Pos -> Lexeme -> Parser ()
putBack pos l = Parser $ \ _ stream -> Right [((),pos,(l:stream))]

satisfy :: (Lexeme -> Bool) -> Parser Lexeme
satisfy f = core <* (whiteSpace <||> eof)
    where
        core = Parser $ \ pos stream ->
                    case stream of
                        (x:xs) ->
                            if f x
                                then Right [(x, advancePos x pos, xs)]
                                else
                                    let msg = "parsing error in \"" <> lexemeFace x <> "\"."
                                    in  Left $ addToTop msg []
                        _ -> Left $ Nested Nothing []

advancePos :: Lexeme -> Pos -> Pos
advancePos L_Newline (Pos fp line _  ) = Pos fp (line + 1) 1
advancePos l         (Pos fp line col) = Pos fp  line      (col + lexemeWidth l)

inCompleteFile :: Parser a -> Parser a
inCompleteFile parser = do
    whiteSpace
    result <- parser
    eof
    return result

eof :: Parser ()
eof = Parser $ \ pos stream ->
    case stream of
        []    -> Right [((), pos, stream)]
        (s:_) -> let msg = showPos pos <+> "parsing error in \"" <> lexemeFace s <> "\"."
                 in  Left $ addToTop msg []

isWhiteSpace :: Lexeme -> Bool
isWhiteSpace L_Space     = True
isWhiteSpace L_Tab       = True
isWhiteSpace L_Newline   = True
isWhiteSpace LComment {} = True
isWhiteSpace _           = False

whiteSpace :: Parser ()
whiteSpace = flip (<?>) "white space" $ Parser $ \ pos ns ->
                let
                    (consumed,rest) = span isWhiteSpace ns
                    newPos = foldr advancePos pos (reverse consumed)
                in
                    Right [((), newPos, rest)]


lexeme :: Lexeme -> Parser ()
lexeme l = void (satisfy (l==)) <?> "expecting" <+> lexemeFace l <> ","

lexeme' :: Lexeme -> Parser Lexeme
lexeme' l = l <$ lexeme l

identifier :: Parser String
identifier = T.unpack <$> identifierText

identifierText :: Parser T.Text
identifierText = do LIdentifier i <- satisfy isIdentifier; return i
    where isIdentifier LIdentifier {} = True
          isIdentifier _ = False

integer :: Parser Integer
integer = do LIntLiteral i <- satisfy isInt; return i
    where isInt LIntLiteral {} = True
          isInt _ = False

comma :: Parser ()
comma = lexeme L_Comma <?> "comma"

dot :: Parser ()
dot = lexeme L_Dot <?> "dot"

colon :: Parser ()
colon = lexeme L_Colon <?> "colon"

sepBy :: Parser a -> Parser () -> Parser [a]
-- sepBy a sep = many (a <* sep)
sepBy a sep = sepBy1 a sep <||> return []

sepBy1 :: Parser a -> Parser () -> Parser [a]
-- sepBy1 a sep = some (a <* sep)
sepBy1 a sep = do
    x <- a
    ( do sep; xs <- sepBy a sep; return (x:xs) ) <||> return [x]

-- parses a specified number of elements separated by the given separator
countSep :: Int -> Parser a -> Parser sep -> Parser [a]
countSep 1 p _   = (:[]) <$> p
countSep i p sep | i > 1 = (:) <$> (p <* sep) <*> countSep (i-1) p sep
countSep _ _ _   = return []

-- parses at least a given number of elements separated by the given separator
countSepAtLeast :: Int -> Parser a -> Parser sep -> Parser [a]
countSepAtLeast i p sep = (++) <$> countSep i p sep <*> many (sep *> p)


optionMaybe :: Parser a -> Parser (Maybe a)
optionMaybe p = Just <$> p <||> return Nothing

between :: Parser () -> Parser () -> Parser a -> Parser a
between before after inner = do
    before
    result <- inner
    after
    return result

betweenTicks :: Parser a -> Parser a
betweenTicks = between (lexeme L_BackTick) (lexeme L_BackTick)

parens :: Parser a -> Parser a
parens = between (lexeme L_OpenParen) (lexeme L_CloseParen)

braces :: Parser a -> Parser a
braces = between (lexeme L_OpenCurly) (lexeme L_CloseCurly)

brackets :: Parser a -> Parser a
brackets = between (lexeme L_OpenBracket) (lexeme L_CloseBracket)

chainL :: Parser (a -> a -> a) -> Parser a -> Parser a
chainL op p = do x <- p; rest x
    where
        rest x = ( do f <- op; y <- p; rest (f x y) )
               <||> return x

chainR :: Parser (a -> a -> a) -> Parser a -> Parser a
chainR op p = scan
    where
        scan = do x <- p; rest x

        rest x = ( do f <- op; y <- scan; return (f x y) )
               <||> return x

chainN :: Parser (a -> a -> a) -> Parser a -> Parser a
chainN op p = do
    a <- p
    o <- op
    b <- p
    return (o a b)

test :: Parser [Lexeme]
test = do
    let one = do
            whiteSpace
            lexeme' L_allDiff <|> lexeme' L_defined
    is <- some one
    eof
    return is
    -- whiteSpace
    -- 
    -- -- whiteSpace
    -- lexeme L_allDiff <|> lexeme L_defined                               <?> "second"
    -- whiteSpace
    -- lexeme L_allDiff <|> lexeme L_defined                               <?> "third"
    -- eof

next2 :: Parser [Lexeme]
next2 = do
    i <- next
    j <- next
    return [i,j]

next3 :: Parser [Lexeme]
next3 = do
    i <- next
    j <- next
    k <- next
    return [i,j,k]

lexAndParse ::
    ( Applicative m
    , MonadError (Nested Doc) m
    ) => Maybe FilePath -> Parser a -> T.Text -> m [a]
lexAndParse mfp parser string = do
    lexemes <- runLexer string
    results <- case runParser parser (Pos mfp 1 1) lexemes of Left err -> throwError err; Right r -> return r
    return [ a | (a,_,_) <- results ]

parseEither :: Show a => Parser a -> String -> Either (Nested Doc) a
parseEither p s = case lexAndParse Nothing p (T.pack s) of
    Left msg  -> Left msg
    Right [a] -> Right a
    Right as  -> Left $ addToTop "Unknown parsing error." (map (flip addToTop [] . text . show) as)

unsafeParse :: Show a => Parser a -> String -> a
unsafeParse p s = case parseEither p s of
    Left  msg -> error $ show msg
    Right a   -> a

lexAndParseIO :: Show a => Parser a -> T.Text -> IO [a]
lexAndParseIO parser string = case lexAndParse Nothing parser string of
    Left msg -> do
        putStrLn $ renderDoc $ nestedToDoc msg
        return []
    Right as -> do
        -- mapM_ ppPrint as
        return as


-- -- runParser :: MonadError (Nested Doc) m => Parser a -> Stream -> m [a]
-- -- runParser p ls = case P.papply' p () ls of
-- --     Left msg -> throwError msg
-- --     Right rs -> return [ r
-- --                        | (r,(),rest) <- rs
-- --                        -- , null rest
-- --                        ]
-- -- 
-- -- reserved :: Lexeme -> Parser ()
-- -- reserved l = do
-- --     i <- P.item
-- --     if i == l
-- --         then return ()
-- --         else failMsg $ "Expecting lexeme: " ++ show l
-- -- 
-- -- lexeme :: Parser Lexeme
-- -- lexeme = P.item
-- -- 
-- -- pTwo :: Parser (Lexeme, Lexeme)
-- -- pTwo = do
-- --     i <- lexeme
-- --     j <- lexeme
-- --     return (i,j)
-- -- 
-- -- pOne :: Parser (Lexeme, Lexeme)
-- -- pOne = do
-- --     i <- lexeme
-- --     return (i,i)
-- -- 
-- -- 
-- -- -- test :: Parser (Lexeme, Lexeme) -> T.Text -> IO ()
-- -- -- test p t = case lexAndParse p t of
-- -- --         Left msg -> error $ show msg
-- -- --         Right r  -> ppPrint r
