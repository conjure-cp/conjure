{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE DeriveGeneric #-}

module Language.E.Lexer where

import Conjure.Prelude

import Data.Char ( isAlpha, isAlphaNum )
import qualified Data.HashMap.Strict as M
import qualified Data.Text as T
import qualified Data.Text.Read as T
import qualified Text.PrettyPrint as Pr

import Text.Parsec.Pos ( SourcePos, initialPos, incSourceLine, incSourceColumn, setSourceColumn )


type LexemePos = (Lexeme, SourcePos)

data Lexeme
    = LIntLiteral Integer
    | LIdentifier T.Text
    | LMetaVar T.Text
    | LComment T.Text

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

    -- type: relation
    | L_relation

    -- type: partition
    | L_partition
    | L_regular
    | L_complete
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
    | L_image
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

    -- global constraints
    | L_allDiff
    | L_dontCare

    -- matrix only operators
    | L_flatten
    | L_normIndices

    -- in the rule language
    | L_lambda
    | L_quantifier
    | L_representation

    -- arithmetic operators

    | L_Plus                --    +           -- sum, infix : (int,int) -> int
    | L_Minus               --    -           -- (subtraction, infix : (int,int) -> int) OR (unary minus : int -> int)
    | L_Times               --    *           -- multiplication, infix : (int,int) -> int
    | L_Div                 --    /           -- integer division, infix
    | L_Mod                 --    %           -- module, infix
    | L_Pow                 --    **          -- exponentiation, infix : (int,int) -> int
    -- | L_Factorial           --    !           -- factorial, postfix : int -> int

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

    deriving (Eq, Ord, Show, Generic)

instance Hashable Lexeme

lexemeText :: Lexeme -> T.Text
lexemeText l = T.pack $ show (lexemeFace l)

textToLexeme :: T.Text -> Maybe Lexeme
textToLexeme t = M.lookup t mapTextToLexeme

lexemeFace :: Lexeme -> Pr.Doc
lexemeFace L_Newline = "new line"
lexemeFace L_Space   = "space character"
lexemeFace L_Tab     = "tab character"
lexemeFace (LIntLiteral i) = Pr.integer i
lexemeFace (LIdentifier i) = Pr.text (T.unpack i)
lexemeFace (LComment    i) = Pr.text (T.unpack i)
lexemeFace l = case M.lookup l mapLexemeToText of
    Nothing -> Pr.text (show l)
    Just t  -> Pr.text (T.unpack t)

lexemeWidth :: Lexeme -> Int
lexemeWidth L_Tab = 4
lexemeWidth (LIntLiteral i) = length (show i)
lexemeWidth (LIdentifier i) = T.length i
lexemeWidth (LComment    i) = T.length i
lexemeWidth l = case lookup l (map swap lexemes) of
    Nothing -> 0
    Just t  -> T.length t

mapTextToLexeme :: M.HashMap T.Text Lexeme
mapTextToLexeme = M.fromList lexemes

mapLexemeToText :: M.HashMap Lexeme T.Text
mapLexemeToText = M.fromList $ map swap lexemes

lexemes :: [(T.Text, Lexeme)]
lexemes = reverse $ sortBy ( comparing (T.length . fst) ) $ map swap
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

    , ( L_bool, "bool" )
    , ( L_false, "false" )
    , ( L_true, "true" )
    , ( L_int, "int" )
    , ( L_new, "new" )
    , ( L_type, "type" )
    , ( L_enum, "enum" )
    , ( L_tuple, "tuple" )
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
    , ( L_relation, "relation" )
    , ( L_partition, "partition" )
    -- , ( L_regular, "regular" )
    -- , ( L_complete, "complete" )
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
    , ( L_image, "image" )
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
    , ( L_allDiff, "allDiff" )
    , ( L_dontCare, "dontCare" )
    , ( L_flatten, "flatten" )
    , ( L_normIndices, "normIndices" )
    , ( L_lambda, "lambda" )
    , ( L_quantifier, "quantifier" )
    , ( L_representation, "representation" )
    , ( L_Plus            , "+"     )
    , ( L_Minus           , "-"     )
    , ( L_Times           , "*"     )
    , ( L_Div             , "/"     )
    , ( L_Mod             , "%"     )
    , ( L_Pow             , "**"    )
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
    -- , ( L_Newline         , "\r"    )
    -- , ( L_Newline         , "\n\r"  )
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

    ]

runLexer :: (Applicative m, MonadError Pr.Doc m) => T.Text -> m [LexemePos]
runLexer text = do
    ls <- go text
    let lsPaired = calcPos (initialPos "") ls
    return $ removeSpaces lsPaired
    where
        go t = do
            let results = catMaybes $  tryLexMetaVar t
                                    :  map (tryLex t) lexemes
                                    ++ [ tryLexIntLiteral t
                                       , tryLexIden t
                                       , tryLexComment t
                                       ]
            if T.null t
                then return []
                else case results of
                        [] -> throwError $ "Lexing error:" Pr.<+> Pr.text (T.unpack t)
                        ((rest,lexeme):_) -> (lexeme:) <$> go rest

        calcPos _   []     = []
        calcPos pos (x:xs) = (x, pos) : calcPos (nextPos pos x) xs

        nextPos pos L_Newline = incSourceLine (setSourceColumn pos 1) 1
        nextPos pos l         = incSourceColumn pos (lexemeWidth l)

removeSpaces :: [LexemePos] -> [LexemePos]
removeSpaces = filter (not . isSpace . fst)
    where
        isSpace L_Newline {} = True
        isSpace L_Tab     {} = True
        isSpace L_Space   {} = True
        isSpace LComment  {} = True
        isSpace _            = False

tryLex :: T.Text -> (T.Text, Lexeme) -> Maybe (T.Text, Lexeme)
tryLex running (face,lexeme) = case T.stripPrefix face running of
    Nothing   -> Nothing
    Just rest ->
        if T.all isIdentifierLetter face
            then case T.uncons rest of
                        Just (ch, _) | isIdentifierLetter ch -> Nothing
                        _                                    -> Just (rest, lexeme)
            else Just (rest, lexeme)

tryLexIntLiteral :: T.Text -> Maybe (T.Text, Lexeme)
tryLexIntLiteral t = case T.decimal t of
    Left _ -> Nothing
    Right (x,rest) -> Just (rest, LIntLiteral x)

isIdentifierLetter :: Char -> Bool
isIdentifierLetter ch = isAlphaNum ch || ch `elem` "_'#§~"

tryLexMetaVar :: T.Text -> Maybe (T.Text, Lexeme)
tryLexMetaVar running =
    case T.uncons running of
        Just ('&',rest) -> do
            (rest2, LIdentifier iden) <- tryLexIden rest
            return (rest2, LMetaVar iden)
        _ -> Nothing

tryLexIden :: T.Text -> Maybe (T.Text, Lexeme)
tryLexIden running =
    let
        (iden,rest) = T.span isIdentifierLetter running
    in
        case T.uncons running of
            Just (ch,_) | isAlpha ch || ch `elem` "_'#§~" ->
                if T.null iden
                    then Nothing
                    else Just (rest, LIdentifier iden)
            _ -> Nothing

tryLexComment :: T.Text -> Maybe (T.Text, Lexeme)
tryLexComment running = let (dollar,rest1) = T.span (=='$') running
                        in  if T.null dollar
                                then Nothing
                                else let (commentLine,rest2) = T.span (/='\n') rest1
                                     in  Just (rest2, LComment commentLine)

