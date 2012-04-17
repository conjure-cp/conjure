{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.OpDescriptor where

import Control.Arrow ( second )

import Language.EssenceLexer
import Language.EssenceLexerP
import ParsePrint
import PrintUtils ( (<+>), (<>), text )
import qualified PrintUtils as Pr

import Language.Essence.Domain
import Language.Essence.Expr
import Language.Essence.Op
import Language.Essence.Range


isLeftAssoc :: Op -> Bool
isLeftAssoc op = case opDescriptor op of
    OpInfix (_, InfixL, _) _ -> True
    _ -> False

isRightAssoc :: Op -> Bool
isRightAssoc op = case opDescriptor op of
    OpInfix (_, InfixR, _) _ -> True
    _ -> False



data Fixity = InfixL | InfixN | InfixR deriving Eq

-- will be used while parsing and pretty-printing operators
data OpDescriptor
    = OpLispy
            (Parser Expr)
            ([Expr] -> Pr.Doc)
    | OpInfix
            (Int, Fixity, Parser (Expr -> Expr -> Expr))
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

        pFace :: Op -> Parser ()
        pFace op = case opFace op of
                    Nothing -> error $ "opFace " ++ show op
                    Just f  -> lexeme f

        prFace :: Op -> String;
        prFace op = case opFace op of
                    Nothing -> error $ "opFace " ++ show op
                    Just f  -> show $ lexemeFace f

        genLispy :: Op -> [Int] -> OpDescriptor
        genLispy op cards = OpLispy
            ( do
                pFace op <?> "expecting" <+> maybe Pr.empty lexemeFace (opFace op)
                is <- parens (parse `sepBy1` comma)
                if length is `elem` cards
                    then return $ EOp op is
                    else fail ("Unexpected number of arguments in " ++ prFace op)
            )
            (\ xs -> text (prFace op) <> prettyList Pr.parens Pr.comma xs )

        genInfix :: Op -> Int -> Fixity -> OpDescriptor
        genInfix op prec assoc = OpInfix
            ( prec
            , assoc
            , do pFace op
                 return $ \ x y -> EOp op [x,y]
            )
            ( \ prettyPrec envPrec x y -> case assoc of
                    InfixL     -> Pr.parensIf (envPrec > prec) $ Pr.sep [ prettyPrec  prec    x
                                                                        , text $ prFace op
                                                                        , prettyPrec (prec+1) y
                                                                        ]
                    InfixN     -> Pr.parensIf (envPrec > prec) $ Pr.sep [ prettyPrec (prec+1) x
                                                                        , text $ prFace op
                                                                        , prettyPrec (prec+1) y
                                                                        ]
                    InfixR     -> Pr.parensIf (envPrec > prec) $ Pr.sep [ prettyPrec (prec+1) x
                                                                        , text $ prFace op
                                                                        , prettyPrec  prec    y
                                                                        ]
            )

        genPrefix :: Op -> OpDescriptor
        genPrefix op = OpPrefix
            ( do pFace op
                 return $ \ x -> EOp op [x]
            )
            ( \ x -> text (prFace op) <> Pr.parensIf (not $ isAtomicExpr x) (pretty x) )

        genPostfix :: Op -> OpDescriptor
        genPostfix op = OpPostfix
            ( do pFace op
                 return $ \ x -> EOp op [x]
            )
            ( \ x -> Pr.parensIf (not $ isAtomicExpr x) (pretty x) <> text (prFace op) )

        helper :: Op -> OpDescriptor
        helper op@Plus         = genInfix    op   600  InfixL
        helper op@Minus        = genInfix    op   600  InfixL
        helper op@Times        = genInfix    op   700  InfixL
        helper op@Div          = genInfix    op   700  InfixL
        helper op@Mod          = genInfix    op   700  InfixL
        helper op@Pow          = genInfix    op   800  InfixR
        helper op@Negate       = genPrefix   op
        helper op@Factorial    = genPostfix  op
        helper op@Lt           = genInfix    op   400  InfixN
        helper op@Leq          = genInfix    op   400  InfixN
        helper op@Gt           = genInfix    op   400  InfixN
        helper op@Geq          = genInfix    op   400  InfixN
        helper op@Neq          = genInfix    op   400  InfixN
        helper op@Eq           = genInfix    op   400  InfixN
        helper op@Not          = genPrefix   op
        helper op@Or           = genInfix    op   110  InfixR
        helper op@And          = genInfix    op   120  InfixR
        helper op@Imply        = genInfix    op    50  InfixN
        helper op@Iff          = genInfix    op    50  InfixN
        helper op@Union        = genInfix    op   200  InfixL
        helper op@Intersect    = genInfix    op   300  InfixL
        helper op@Subset       = genInfix    op   400  InfixN
        helper op@SubsetEq     = genInfix    op   400  InfixN
        helper op@Supset       = genInfix    op   400  InfixN
        helper op@SupsetEq     = genInfix    op   400  InfixN
        helper op@In           = genInfix    op   150  InfixN
        helper op@Max          = genLispy    op   [1,2]
        helper op@Min          = genLispy    op   [1,2]
        helper op@ToSet        = genLispy    op   [1]
        helper op@ToMSet       = genLispy    op   [1]
        helper op@ToRelation   = genLispy    op   [1]
        helper op@Defined      = genLispy    op   [1]
        helper op@Range        = genLispy    op   [1]
        helper op@Image        = genLispy    op   [2]
        helper op@PreImage     = genLispy    op   [2]
        helper op@Inverse      = genLispy    op   [2]
        helper op@Together     = genLispy    op   [3]
        helper op@Apart        = genLispy    op   [3]
        helper op@Party        = genLispy    op   [2]
        helper op@Participants = genLispy    op   [1]
        helper op@Parts        = genLispy    op   [1]
        helper op@Freq         = genLispy    op   [2]
        helper op@Hist         = genLispy    op   [2]
        helper op@HasType      = genInfix    op    1000 InfixN
        helper op@HasDomain    = genInfix    op    1000 InfixN
        helper op@AllDiff      = genLispy    op   [1]
        helper op@ToInt        = genLispy    op   [1]
        helper op@Flatten      = genLispy    op   [1,2]
        helper op@NormIndices  = genLispy    op   [1]
        helper TwoBars         = OpSpecial (pa <??> "|expression|") pr
            where
                pa = between (lexeme L_Bar) (lexeme L_Bar) $ do i <- parse; return $ EOp TwoBars [i]
                pr (EOp TwoBars [x]) = "|" <> pretty x <> "|"
                pr x = error $ "pretty TwoBars: " ++ show x

        helper Index           = OpPostfix (pa <?> "indexed expression") pr
            where
                pa = do
                    let pIndexer = pRList <||> parse
                        pRList   = do
                            i <- optionMaybe parse
                            dot; dot
                            j <- optionMaybe parse
                            return $ D $ DInt $ RFromTo [Right (i,j)]
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
        helper Replace = OpPostfix pa pr
            where
                pa = braces $ do
                    ijs <- do i <- parse
                              lexeme L_LongArrow
                              j <- parse
                              return (i,j)
                           `sepBy1` comma
                    let
                        go x []           = x
                        go x ((i,j):rest) = go (EOp Replace [x,i,j]) rest
                    return $ \ x -> go x ijs
                pr (EOp Replace [a,b,c]) =
                    (if isAtomicExpr a then id else Pr.parens) (pretty a)
                    <+> Pr.braces (pretty b <+> "-->" <+> pretty c)
                pr x = error $ "pretty Replace: " ++ show x
