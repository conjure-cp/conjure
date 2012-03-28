{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.OpDescriptor where

import Control.Applicative
import Control.Arrow ( second )
import Control.Monad.Identity
import Data.Char ( isLetter, isNumber )

import ParsecUtils
import ParsePrint
import PrintUtils ( (<+>), (<>), text )
import qualified PrintUtils as Pr

import Language.Essence.Domain
import Language.Essence.Expr
import Language.Essence.Op
import Language.Essence.Range



type OperatorParser = Operator String () Identity Expr

data Fixity = InfixL | InfixN | InfixR

-- will be used while parsing and pretty-printing operators
data OpDescriptor
    = OpLispy
            (Parser Expr)
            ([Expr] -> Pr.Doc)
    | OpInfix
            (Int, OperatorParser)
            ((Int -> Expr -> Pr.Doc) -> Int -> Expr -> Expr -> Pr.Doc)
    | OpPrefix
            (Parser (Expr -> Expr))
            (Expr -> Pr.Doc)
    | OpPostfix
            (Parser (Expr -> Expr))
            (Expr -> Pr.Doc)
    | OpSpecial
            (Parser Expr)
            (Expr -> Pr.Doc)

opDescriptor :: Op -> OpDescriptor
opDescriptor = helper
    where
        pFace :: String -> Parser ()
        pFace s
            | all (\ i -> isLetter i || isNumber i || i == '_' ) s = reserved s <?> "operator"
            | otherwise = reservedOp s <?> "operator"

        genLispy :: Op -> Int -> OpDescriptor
        genLispy op cardinality = OpLispy
            ( do
                reserved (opFace op)
                is <- parens (countSep cardinality parse comma)
                return $ EOp op is
            )
            ( \ xs -> text (opFace op) <> prettyList Pr.parens Pr.comma xs)

        genInfix :: Op -> Int -> Assoc -> OpDescriptor
        genInfix op prec assoc = OpInfix
            ( prec
            , Infix ( do pFace (opFace op)
                         return $ \ x y -> EOp op [x,y]
                    )
                    assoc
            )
            ( \ prettyPrec envPrec x y -> case assoc of
                    AssocLeft  -> Pr.parensIf (envPrec < prec) $ prettyPrec  prec    x <+> text (opFace op) <+> prettyPrec (prec-1) y
                    AssocNone  -> Pr.parensIf (envPrec < prec) $ prettyPrec (prec-1) x <+> text (opFace op) <+> prettyPrec (prec-1) y
                    AssocRight -> Pr.parensIf (envPrec < prec) $ prettyPrec (prec-1) x <+> text (opFace op) <+> prettyPrec  prec    y
            )

        genPrefix :: Op -> Int -> OpDescriptor
        genPrefix op _prec = OpPrefix
            ( do pFace (opFace op)
                 return $ \ x -> EOp op [x]
            )
            ( \ x -> text (opFace op) <> Pr.parensIf (not $ isAtomicExpr x) (pretty x) )

        genPostfix :: Op -> Int -> OpDescriptor
        genPostfix op _prec = OpPostfix
            ( do pFace (opFace op)
                 return $ \ x -> EOp op [x]
            )
            ( \ x -> Pr.parensIf (not $ isAtomicExpr x) (pretty x) <> text (opFace op) )

        helper :: Op -> OpDescriptor
        helper op@Plus         = genInfix    op   400  AssocLeft
        helper op@Minus        = genInfix    op   400  AssocLeft
        helper op@Times        = genInfix    op   300  AssocLeft
        helper op@Div          = genInfix    op   300  AssocLeft
        helper op@Mod          = genInfix    op   300  AssocLeft
        helper op@Pow          = genInfix    op   200  AssocRight
        helper op@Abs          = genLispy    op     1
        helper op@Negate       = genPrefix   op    50
        helper op@Factorial    = genPostfix  op   100
        helper op@Lt           = genInfix    op   800  AssocNone
        helper op@Leq          = genInfix    op   800  AssocNone
        helper op@Gt           = genInfix    op   800  AssocNone
        helper op@Geq          = genInfix    op   800  AssocNone
        helper op@Neq          = genInfix    op   800  AssocNone
        helper op@Eq           = genInfix    op   800  AssocNone
        helper op@Not          = genPrefix   op  1300
        helper op@Or           = genInfix    op  1000  AssocLeft
        helper op@And          = genInfix    op   900  AssocLeft
        helper op@Imply        = genInfix    op  1100  AssocNone
        helper op@Iff          = genInfix    op  1200  AssocNone
        helper op@Union        = genInfix    op   300  AssocLeft
        helper op@Intersect    = genInfix    op   300  AssocLeft
        helper op@Subset       = genInfix    op   700  AssocNone
        helper op@SubsetEq     = genInfix    op   700  AssocNone
        helper op@Supset       = genInfix    op   700  AssocNone
        helper op@SupsetEq     = genInfix    op   700  AssocNone
        helper op@Card         = genLispy    op     1
        helper op@Elem         = genInfix    op   700  AssocNone
        helper op@Max          = genLispy    op     1
        helper op@Min          = genLispy    op     1
        helper op@ToSet        = genLispy    op     1
        helper op@ToMSet       = genLispy    op     1
        helper op@ToRel        = genLispy    op     1
        helper op@Defined      = genLispy    op     1
        helper op@Range        = genLispy    op     1
        helper op@Image        = genLispy    op     2
        helper op@PreImage     = genLispy    op     2
        helper op@Inverse      = genLispy    op     2
        helper op@Together     = genLispy    op     3
        helper op@Apart        = genLispy    op     3
        helper op@Party        = genLispy    op     2
        helper op@Participants = genLispy    op     1
        helper op@Parts        = genLispy    op     1
        helper op@Freq         = genLispy    op     2
        helper op@Hist         = genLispy    op     2
        helper Index           = OpPostfix pa pr
            where
                pa = do
                    let pIndexer = try pRList <|> parse
                        pRList   = do
                            i <- optionMaybe parse
                            dot; dot
                            j <- optionMaybe parse
                            return $ D $ DInt $ RFromTo i j
                    is <- brackets $ pIndexer `sepBy1` comma
                    return (\ x -> foldl (\ m' i -> EOp Index [m', i]) x is)
                pr (EOp Index [m,i]) =
                    let
                        f (EOp Index [x,y]) = second (prettyIndexProject y:) (f x)
                        f x = (x,[])
                        (a,bs) = f m
                    in
                        pretty a <> prettyListDoc Pr.brackets Pr.comma (reverse (prettyIndexProject i:bs))
                pr x = error $ "pretty Index: " ++ show x
                prettyIndexProject (D (DInt i)) = pretty i
                prettyIndexProject i = pretty i
        helper op@HasType   = genInfix op 1500 AssocNone
        helper op@HasDomain = genInfix op 1500 AssocNone
        helper Replace = OpPostfix pa pr
            where
                pa = braces $ do
                    ijs <- do i <- parse
                              reservedOp "->"
                              j <- parse
                              return (i,j)
                           `sepBy1` comma
                    let
                        go x []           = x
                        go x ((i,j):rest) = go (EOp Replace [x,i,j]) rest
                    return $ \ x -> go x ijs
                pr (EOp Replace [a,b,c]) =
                    (if isAtomicExpr a then id else Pr.parens) (pretty a)
                    <+> Pr.braces (pretty b <+> "->" <+> pretty c)
                pr x = error $ "pretty Replace: " ++ show x
        helper op@AllDiff = genLispy op 1
