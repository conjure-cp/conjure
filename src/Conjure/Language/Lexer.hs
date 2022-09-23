{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE InstanceSigs #-}
{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use camelCase" #-}

module Conjure.Language.Lexer
    ( Lexeme(..)
    , LexemePos(..)
    , runLexer
    , textToLexeme
    , lexemeText
    , lexemeFace
    ) where
import Conjure.Prelude hiding (some,many)

import Data.Char ( isAlpha, isAlphaNum )
import Data.Void
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Text.PrettyPrint as Pr
import qualified Text.Megaparsec.Char.Lexer as L
import Text.Megaparsec.Char

import Text.Megaparsec --( SourcePos, initialPos, incSourceLine, incSourceColumn, setSourceColumn )
import Text.Megaparsec.Stream


data LexemePos = LexemePos
                    Lexeme          -- the lexeme
                    SourcePos       -- source position, the beginning of this lexeme
                    SourcePos       -- source position, just after this lexeme, including whitespace after the lexeme
    deriving (Show,Eq, Ord)

data Lexeme
    = LIntLiteral Integer
    | LMissingIntLiteral   --helper for missing symbol
    | LIdentifier T.Text
    | LMissingIdentifier --helper for missing symbol
    | LMetaVar T.Text
    | LMissingMetaVar --helper for missing symbol
    -- general
    | L_be
    | L_from
    | L_of
    | L_domain

    | L_language
    | L_dim
    | L_find
    | L_given
    | L_letting
    | L_where
    | L_such
    | L_that
    | L_minimising
    | L_maximising
    | L_branching
    | L_on
    | L_heuristic

    -- type: boolean
    | L_bool
    | L_false
    | L_true

    -- type: integer
    | L_int

    -- creating a new type
    | L_new
    | L_type
    | L_enum

    -- type tuple
    | L_tuple

    -- type record
    | L_record

    -- type variant
    | L_variant
    | L_active

    -- type: matrix
    | L_matrix
    | L_indexed
    | L_by

    -- type set
    | L_set
    | L_size
    | L_minSize
    | L_maxSize

    -- type: mset
    | L_mset
    | L_minOccur
    | L_maxOccur

    -- type: function
    | L_function
    | L_total
    | L_partial
    | L_injective
    | L_surjective
    | L_bijective

    -- type: sequence
    | L_sequence

    -- type: relation
    | L_relation

    -- type: partition
    | L_partition
    | L_regular
    | L_partSize
    | L_minPartSize
    | L_maxPartSize
    | L_numParts
    | L_minNumParts
    | L_maxNumParts

    -- operators, page 21 of the holy paper
    | L_union
    | L_intersect
    | L_subset
    | L_subsetEq
    | L_supset
    | L_supsetEq
    | L_in
    | L_max
    | L_min
    | L_toSet
    | L_toMSet
    | L_toRelation
    | L_defined
    | L_range
    | L_restrict
    | L_image
    | L_imageSet
    | L_preImage
    | L_inverse
    | L_together
    | L_apart
    | L_party
    | L_participants
    | L_parts
    | L_freq
    | L_hist

    | L_toInt
    | L_makeTable
    | L_table

    -- global constraints
    | L_allDiff
    | L_alldifferent_except
    | L_gcc
    | L_atleast
    | L_atmost

    | L_dontCare

    | L_catchUndef

    -- matrix only operators
    | L_flatten
    | L_concatenate
    | L_normIndices

    -- in the rule language
    -- | L_lambda
    -- | L_quantifier
    -- | L_representation

    -- arithmetic operators

    | L_Plus                --    +           -- sum, infix : (int,int) -> int
    | L_Minus               --    -           -- (subtraction, infix : (int,int) -> int) OR (unary minus : int -> int)
    | L_Times               --    *           -- multiplication, infix : (int,int) -> int
    | L_Div                 --    /           -- integer division, infix
    | L_Mod                 --    %           -- modulo, infix
    | L_Pow                 --    **          -- exponentiation, infix : (int,int) -> int
    | L_factorial

    -- equality

    | L_Eq                  --    =           -- equals, infix.
    | L_Neq                 --    !=          -- not-equals, infix

    -- comparison

    | L_Lt                  --    <           -- less-than, infix.
    | L_Leq                 --    <=          -- less-than-or-eq, infix.
    | L_Gt                  --    >           -- greater-than, infix.
    | L_Geq                 --    >=          -- greater-than-or-eq, infix.

    -- logical operators

    | L_And                 --    /\          -- logical-and, infix
    | L_Or                  --    \/          -- logical-or, infix.
    | L_Imply               --    ->          -- implication, infix
    | L_Iff                 --    <->         -- iff, infix.
    -- | L_Not                 --    !           -- negation, prefix
    | L_ExclamationMark     -- for poth L_Factorial and L_ExclamationMark

    -- the function arrow

    | L_LongArrow           --    -->         -- function domains and constants

    -- in rule language

    | L_Colon               --    :           -- has-domain, infix, (expr,domain) -> bool. also does pattern matching.
    | L_DoubleColon         --    ::          -- has-type, infix, (expr,type) -> bool. also does pattern matching.
    | L_At                  --    @           -- bubble operator.

    -- lex operators

    | L_LexGeq              --    >=lex
    | L_LexGt               --    >lex
    | L_LexLt               --    <=lex
    | L_LexLeq              --    <lex

    -- for "abs" and "card"
    | L_Bar                 --    |

    -- attaching a type to an expression
    | L_BackTick            --    `

    -- others
    | L_Dot
    | L_Comma
    | L_SemiColon

    | L_OpenParen
    | L_CloseParen
    | L_OpenBracket
    | L_CloseBracket
    | L_OpenCurly
    | L_CloseCurly

    | L_Newline
    | L_Carriage
    | L_Space
    | L_Tab

    | L_SquigglyArrow
    | L_CaseSeparator

    | L_HasRepr
    | L_HasType
    | L_HasDomain
    | L_indices

    | L_DotLt
    | L_DotLeq
    | L_DotGt
    | L_DotGeq

    | L_TildeLt
    | L_TildeLeq
    | L_TildeGt
    | L_TildeGeq

    | L_LeftArrow

    | L_subsequence
    | L_substring
    | L_powerSet

    | L_pred
    | L_succ

    -- type functional
    | L_transform
    | L_EOF

    deriving (Eq, Ord, Show,Generic) 
instance Hashable  Lexeme

--     deriving (Eq, Ord, Show, Generic)

-- instance Hashable Lexeme

lexemeText :: Lexeme -> T.Text
lexemeText l = T.pack $ show (lexemeFace l)

textToLexeme :: T.Text -> Maybe Lexeme
textToLexeme t = M.lookup t mapTextToLexeme

lexemeFace :: Lexeme -> Pr.Doc
lexemeFace L_Newline = "new line"
lexemeFace L_Carriage = "\\r"
lexemeFace L_Space   = "space character"
lexemeFace L_Tab     = "tab character"
lexemeFace (LIntLiteral i) = Pr.integer i
lexemeFace (LIdentifier i) = Pr.text (T.unpack i)
-- lexemeFace (LComment    i) = Pr.text (T.unpack i)
lexemeFace l =
    case M.lookup l mapLexemeToText of
        Nothing -> Pr.text (show l)
        Just t  -> Pr.text (T.unpack t)

lexemeWidth :: Lexeme -> Int
lexemeWidth L_Carriage = 0
lexemeWidth L_Tab = 4
lexemeWidth (LIntLiteral i) = length (show i)
lexemeWidth (LIdentifier i) = T.length i
-- lexemeWidth (LComment    i) = T.length i
lexemeWidth l =
    case lookup l (map swap lexemes) of
        Nothing -> 0
        Just t  -> T.length t

mapTextToLexeme :: M.HashMap T.Text Lexeme
mapTextToLexeme = M.fromList lexemes

mapLexemeToText :: M.HashMap Lexeme T.Text
mapLexemeToText = M.fromList $ map swap lexemes

lexemes :: [(T.Text, Lexeme)]
lexemes = sortBy (flip (comparing (T.length . fst))) $ map swap
    [ ( L_be         , "be"         )
    , ( L_from       , "from"       )
    , ( L_of         , "of"         )
    , ( L_domain     , "domain"     )
    , ( L_language   , "language"   )
    , ( L_dim        , "dim"        )
    , ( L_find       , "find"       )
    , ( L_given      , "given"      )
    , ( L_letting    , "letting"    )
    , ( L_where      , "where"      )
    , ( L_such       , "such"       )
    , ( L_that       , "that"       )
    , ( L_minimising , "minimising" )
    , ( L_maximising , "maximising" )
    , ( L_minimising , "minimizing" )
    , ( L_maximising , "maximizing" )
    , ( L_branching  , "branching"  )
    , ( L_on         , "on"         )
    , ( L_heuristic  , "heuristic"  )

    , ( L_bool, "bool" )
    , ( L_false, "false" )
    , ( L_true, "true" )
    , ( L_int, "int" )
    , ( L_new, "new" )
    , ( L_type, "type" )
    , ( L_enum, "enum" )
    , ( L_tuple, "tuple" )
    , ( L_record, "record" )
    , ( L_variant, "variant" )
    , ( L_active, "active" )
    , ( L_matrix, "matrix" )
    , ( L_indexed, "indexed" )
    , ( L_by, "by" )
    , ( L_set, "set" )
    -- , ( L_size, "size" )
    -- , ( L_minSize, "minSize" )
    -- , ( L_maxSize, "maxSize" )
    , ( L_mset, "mset" )
    -- , ( L_minOccur, "minOccur" )
    -- , ( L_maxOccur, "maxOccur" )
    , ( L_function, "function" )
    -- , ( L_total, "total" )
    -- , ( L_partial, "partial" )
    -- , ( L_injective, "injective" )
    -- , ( L_surjective, "surjective" )
    -- , ( L_bijective, "bijective" )
    , ( L_sequence, "sequence" )
    , ( L_relation, "relation" )
    , ( L_partition, "partition" )
    -- , ( L_regular, "regular" )
    -- , ( L_partSize, "partSize" )
    -- , ( L_minPartSize, "minPartSize" )
    -- , ( L_maxPartSize, "maxPartSize" )
    -- , ( L_numParts, "numParts" )
    -- , ( L_minNumParts, "minNumParts" )
    -- , ( L_maxNumParts, "maxNumParts" )
    , ( L_union, "union" )
    , ( L_intersect, "intersect" )
    , ( L_subset, "subset" )
    , ( L_subsetEq, "subsetEq" )
    , ( L_supset, "supset" )
    , ( L_supsetEq, "supsetEq" )
    , ( L_in, "in" )
    , ( L_max, "max" )
    , ( L_min, "min" )
    , ( L_toSet, "toSet" )
    , ( L_toMSet, "toMSet" )
    , ( L_toRelation, "toRelation" )
    , ( L_defined, "defined" )
    , ( L_range, "range" )
    , ( L_restrict, "restrict" )
    , ( L_image, "image" )
    , ( L_imageSet, "imageSet" )
    , ( L_preImage, "preImage" )
    , ( L_inverse, "inverse" )
    , ( L_together, "together" )
    , ( L_apart, "apart" )
    , ( L_party, "party" )
    , ( L_participants, "participants" )
    , ( L_parts, "parts" )
    , ( L_freq, "freq" )
    , ( L_hist, "hist" )
    , ( L_toInt, "toInt" )
    , ( L_makeTable, "makeTable" )
    , ( L_table, "table" )

    , ( L_allDiff, "allDiff" )
    , ( L_alldifferent_except, "alldifferent_except" )
    , ( L_gcc, "gcc" )
    , ( L_atleast, "atleast" )
    , ( L_atmost, "atmost" )

    , ( L_dontCare, "dontCare" )
    , ( L_catchUndef, "catchUndef" )

    , ( L_flatten, "flatten" )
    , ( L_concatenate, "concatenate" )
    , ( L_normIndices, "normIndices" )
    -- , ( L_lambda, "lambda" )
    -- , ( L_quantifier, "quantifier" )
    -- , ( L_representation, "representation" )
    , ( L_Plus            , "+"     )
    , ( L_Minus           , "-"     )
    , ( L_Times           , "*"     )
    , ( L_Div             , "/"     )
    , ( L_Mod             , "%"     )
    , ( L_Pow             , "**"    )
    , ( L_factorial       , "factorial" )
    , ( L_Eq              , "="     )
    , ( L_Neq             , "!="    )
    , ( L_Lt              , "<"     )
    , ( L_Leq             , "<="    )
    , ( L_Gt              , ">"     )
    , ( L_Geq             , ">="    )
    , ( L_And             , "/\\"   )
    , ( L_Or              , "\\/"   )
    , ( L_Imply           , "->"    )
    , ( L_Iff             , "<->"   )
    , ( L_ExclamationMark , "!"     )
    , ( L_LongArrow       , "-->"   )
    , ( L_Colon           , ":"     )
    , ( L_DoubleColon     , "::"    )
    , ( L_At              , "@"     )
    , ( L_LexGeq          , ">=lex" )
    , ( L_LexGt           , ">lex"  )
    , ( L_LexLeq          , "<=lex" )
    , ( L_LexLt           , "<lex"  )
    , ( L_Bar             , "|"     )
    , ( L_BackTick        , "`"     )
    , ( L_Dot             , "."     )
    , ( L_Comma           , ","     )
    , ( L_SemiColon       , ";"     )
    , ( L_OpenParen       , "("     )
    , ( L_CloseParen      , ")"     )
    , ( L_OpenBracket     , "["     )
    , ( L_CloseBracket    , "]"     )
    , ( L_OpenCurly       , "{"     )
    , ( L_CloseCurly      , "}"     )

    , ( L_Newline         , "\n"    )
    , ( L_Carriage        , "\r"    )
    , ( L_Space           , " "     )
    , ( L_Tab             , "\t"    )

    , ( L_SquigglyArrow   , "~~>"   )
    , ( L_CaseSeparator   , "***"   )

    , ( L_HasRepr         , "hasRepr"   )
    , ( L_HasType         , "hasType"   )
    , ( L_HasDomain       , "hasDomain" )
    , ( L_indices         , "indices"   )

    , ( L_DotLt           , ".<"    )
    , ( L_DotLeq          , ".<="   )
    , ( L_DotGt           , ".>"    )
    , ( L_DotGeq          , ".>="   )

    , ( L_TildeLt         , "~<"    )
    , ( L_TildeLeq        , "~<="   )
    , ( L_TildeGt         , "~>"    )
    , ( L_TildeGeq        , "~>="   )

    , ( L_LeftArrow       , "<-"   )

    , ( L_subsequence     , "subsequence"  )
    , ( L_substring       , "substring"    )
    , ( L_powerSet        , "powerSet"     )

    , ( L_pred, "pred" )
    , ( L_succ, "succ" )


    , ( L_transform, "transform")
    ]

mapToLexemePos :: ETok -> LexemePos
mapToLexemePos tok = LexemePos lex start end where
                        (sp,en,_,lex) = offsets tok
                        start = SourcePos  "Test"  (mkPos sp ) (mkPos sp)
                        end = SourcePos "Test"  (mkPos en ) (mkPos en)



runLexer :: MonadFailDoc m => T.Text -> m [LexemePos]
runLexer text = do
    let x = parseMaybe eLex text
    case x of 
        Just y -> return $  mapToLexemePos <$> y
        Nothing -> failDoc "Error"

    
    
-- runLexer text = do
--     ls <- go text
--     let lsPaired = calcPos (initialPos "") ls
--     return lsPaired
--     where
--         go t = do
--             let results = catMaybes $  tryLexMetaVar t
--                                     :  map (tryLex t) lexemes
--                                     ++ [ tryLexIntLiteral t
--                                        , tryLexIden t
--                                        , tryLexQuotedIden t
--                                        , tryLexComment t
--                                        ]
--             if T.null t
--                 then return []
--                 else case results of
--                         [] -> fail ("Lexing error:" Pr.<+> Pr.text (T.unpack t))
--                         ((rest,lexeme):_) -> (lexeme:) <$> go rest

--         -- attach source positions to lexemes
--         -- discard whitespace, but calculate their contribution to source positions
--         calcPos :: SourcePos -> [Lexeme] -> [LexemePos]
--         calcPos _pos [] = []
--         calcPos  pos (this:rest) | isLexemeSpace this                   -- skip if this one is whitespace
--                                  = calcPos (nextPos pos this) rest      -- can only happen at the beginning
--         calcPos  pos (this:rest) =
--             let (restSpaces, restNonSpace) = span isLexemeSpace rest    -- eat up all the whitespace after "this"
--                 pos' = foldl nextPos pos (this:restSpaces)
--             in
--                 if null restNonSpace
--                     then [LexemePos this pos (nextPos pos this)]        -- if this is the last non-whitespace lexeme
--                                                                         -- do not include the whitespace after it
--                     else LexemePos this pos pos' : calcPos pos' restNonSpace

--         nextPos :: SourcePos -> Lexeme -> SourcePos
--         nextPos pos L_Newline  = incSourceLine (setSourceColumn pos 1) 1
--         nextPos pos L_Carriage = pos -- just ignore '\r's
--         nextPos pos l          = incSourceColumn pos (lexemeWidth l)

isLexemeSpace :: Lexeme -> Bool
isLexemeSpace L_Newline {} = True
isLexemeSpace L_Carriage{} = True
isLexemeSpace L_Tab     {} = True
isLexemeSpace L_Space   {} = True
-- isLexemeSpace LComment  {} = True
isLexemeSpace _            = False

tryLex :: T.Text -> (T.Text, Lexeme) -> Maybe (T.Text, Lexeme)
tryLex running (face,lexeme) = do
    rest <- T.stripPrefix face running
    if T.all isIdentifierLetter face
        then
            case T.uncons rest of
                Just (ch, _) | isIdentifierLetter ch -> Nothing
                _                                    -> Just (rest, lexeme)
        else Just (rest, lexeme)

tryLexIntLiteral :: T.Text -> Maybe (T.Text, Lexeme)
tryLexIntLiteral t =
    case T.decimal t of
        Left _ -> Nothing
        Right (x, rest) -> Just (rest, LIntLiteral x)


emojis :: [Char]
emojis = concat [['\x1f600'..'\x1F64F'],
                 ['\x1f300'..'\x1f5ff'],
                 ['\x1f680'..'\x1f999'],
                 ['\x1f1e0'..'\x1f1ff']]


isIdentifierFirstLetter :: Char -> Bool
isIdentifierFirstLetter ch = isAlpha ch || ch `elem` ("_" :: String) || ch `elem` emojis

isIdentifierLetter :: Char -> Bool
isIdentifierLetter ch = isAlphaNum ch || ch `elem` ("_'" :: String) || ch `elem` emojis

tryLexMetaVar :: T.Text -> Maybe (T.Text, Lexeme)
tryLexMetaVar running = do
    ('&', rest) <- T.uncons running
    (rest2, LIdentifier iden) <- tryLexIden rest
    return (rest2, LMetaVar iden)

tryLexIden :: T.Text -> Maybe (T.Text, Lexeme)
tryLexIden running = do
    let (iden,rest) = T.span isIdentifierLetter running
    (ch, _) <- T.uncons running
    if isIdentifierFirstLetter ch
        then
            if T.null iden
                then Nothing
                else Just (rest, LIdentifier iden)
        else Nothing

tryLexQuotedIden :: T.Text -> Maybe (T.Text, Lexeme)
tryLexQuotedIden running = do
    let
        go inp = do
            ('\"', rest) <- T.uncons inp
            go2 "\"" rest

        -- after the first "
        go2 sofar inp = do
            (ch, rest) <- T.uncons inp
            case ch of
                -- end
                '\"'
                    | sofar /= "\""         -- so we don't allow empty strings
                    -> Just (rest, LIdentifier (T.pack (reverse ('\"' : sofar))))
                -- escaped
                '\\' -> do
                    (ch2, rest2) <- T.uncons rest
                    case ch2 of
                        '\"' -> go2 ('\"':sofar) rest2
                        '\\' -> go2 ('\\':sofar) rest2
                        _ -> Nothing
                _ -> go2 (ch:sofar) rest
    go running

-- tryLexComment :: T.Text -> Maybe (T.Text, Lexeme)
-- tryLexComment running = let (dollar,rest1) = T.span (=='$') running
--                         in  if T.null dollar
--                                 then Nothing
--                                 else let (commentLine,rest2) = T.span (/='\n') rest1
--                                      in  Just (rest2, LComment commentLine)


-- instance ShowToken [LexemePos] where
--     showToken = intercalate ", " . map showToken

-- instance ShowToken LexemePos where
--     showToken (LexemePos tok _ _) = showToken tok

-- instance ShowToken Lexeme where
--     showToken = show . lexemeFace

--Generic

-- instance Hashable Lexeme

type Offsets = (Int,Int,Int,Lexeme)
type Parser = Parsec Void T.Text
data Trivia = WhiteSpace T.Text | LineComment T.Text | BlockComment  T.Text
    deriving (Show,Eq,Ord)
data ETok = ETok {
            offsets :: Offsets,
            trivia :: [Trivia],
            lexeme :: Lexeme
}
            deriving (Eq,Ord)


makeToken :: Offsets -> [Trivia] -> Lexeme -> ETok
makeToken = ETok

eLex :: Parser [ETok]
eLex = manyTill aToken eof

aToken :: Parser ETok
aToken = do
            start <- getOffset
            whiteSpace <- pTrivia
            wse <- getOffset
            tok <- aLexeme
            tokenEnd <- getOffset
            return $ makeToken (start,wse,tokenEnd-start,tok) whiteSpace tok

aLexeme :: Parser Lexeme
aLexeme  = try pEOF
    <|> try pNumber
    <|> try (choice $ map pLexeme lexemes)
    <|> try pIdentifier
    <|> try pMetaVar

pEOF :: Parser Lexeme
pEOF = do
        eof
        return L_EOF

pNumber :: Parser Lexeme
pNumber = do
            LIntLiteral <$> L.decimal

pMetaVar :: Parser Lexeme
pMetaVar = do
            empty
            return $ LMetaVar "TODO"


pIdentifier :: Parser Lexeme
pIdentifier = do
                firstLetter <- takeWhile1P Nothing isIdentifierFirstLetter
                rest <- takeWhileP Nothing isIdentifierLetter
                return $ LIdentifier (T.append firstLetter rest)


pLexeme :: (T.Text,Lexeme) -> Parser Lexeme
pLexeme (s,l) = do
                    tok <- string s
                    return l

pTrivia :: Parser [Trivia]
pTrivia = many (whiteSpace <|> lineComment <|> blockComment)

whiteSpace :: Parser Trivia
whiteSpace = do
                s <- some spaceChar
                return $  WhiteSpace $ T.pack s

lineEnd :: Parser ()
lineEnd = do
            _ <- optional eol
            _ <- optional eof
            return ()

lineComment :: Parser Trivia
lineComment = do
                _<-try (chunk "$")
                text <- manyTill L.charLiteral lineEnd
                return $ LineComment $ T.pack text

blockComment :: Parser Trivia
blockComment = do
                _ <- try (chunk "/*")
                text <- manyTill L.charLiteral (chunk "*/")
                return $ BlockComment $ T.pack text

instance Show ETok where
    show (ETok _ _ q) = show q

newtype ETokenStream = ETokenStream [ETok]
instance Stream ETokenStream where
    type Token ETokenStream= ETok
    type Tokens ETokenStream= [ETok]
    tokenToChunk proxy x = [x]
    tokensToChunk proxy xs= xs
    chunkToTokens proxy = id
    chunkLength proxy = length
    chunkEmpty proxy xs = False
    take1_ (ETokenStream (x:xs)) = Just (x, ETokenStream xs)
    take1_ (ETokenStream []) = Nothing
    takeN_ n xs | n<=0 = Just([],xs)
    takeN_ n (ETokenStream []) = Nothing
    takeN_ n (ETokenStream xs) = Just (take n xs,ETokenStream $ drop n xs)
    takeWhile_ p (ETokenStream xs) = (takeWhile p xs,ETokenStream $ dropWhile p xs)
instance VisualStream ETokenStream where
    showTokens p q = concat $ show <$> q
    tokensLength p ls = sum $ len <$> ls
                        where len (ETok (_,_,x,_) _ _) = x

instance TraversableStream ETokenStream where
    reachOffset i s = (Nothing, s)
    reachOffsetNoLine i s = s 