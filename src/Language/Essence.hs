{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
-- {-# LANGUAGE ScopedTypeVariables #-}
-- {-# LANGUAGE TemplateHaskell #-}
-- {-# LANGUAGE TypeOperators #-}

module Language.Essence where

import GenericOps.Coerce
import GenericOps.Core
import ParsecUtils
import ParsePrint
import PrintUtils ( (<+>), (<>), text, Doc )
import Utils
import qualified PrintUtils as Pr

-- import Data.Either
-- import Debug.Trace
-- import Test.HUnit ( Counts, runTestTT )
-- import Test.QuickCheck.All ( forAllProperties )
import Control.Applicative
import Control.Arrow ( first, second )
import Control.Monad.Error
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Writer ( runWriterT )
import Data.Char ( isLetter, isNumber )
import Data.Default ( Default, def )
import Data.String ( IsString, fromString )
import Data.Function ( on )
import Data.Generics ( Data )
import Data.List
import Data.Maybe ( fromMaybe, listToMaybe, mapMaybe, maybeToList )
import Data.Ord ( comparing )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import qualified Data.Map as M
import qualified Data.Set as S
import Test.QuickCheck ( Arbitrary(..), choose, elements )
import Test.QuickCheck.Gen ( oneof )


-- runTests :: IO ()
-- runTests = do
--     print =<< runHUnit
--     print =<< runQC
-- 
-- runHUnit :: IO Counts
-- runHUnit = runTestTT (getParsePrintUnitTests (undefined :: Expr))
-- 
-- runQC :: IO Bool
-- runQC = $forAllProperties runner
--     where
--         runner = quickCheckWithResult stdArgs { maxSize = 2, maxSuccess = 1000 }

-- runQC :: Testable prop => prop -> IO Result
-- runQC = quickCheckWithResult stdArgs { maxSize = 2, maxSuccess = 1000 }


incr :: Value -> Value
incr (VTuple [V (VInt 1),V (VInt 2)]) = VHole $ Identifier "found!"
incr (VInt i) = VInt (i+1)
incr v = v

incrIO :: Value -> IO Value
incrIO (VTuple [V (VInt 1),V (VInt 2)]) = do
    putStrLn "first eq."
    return $ VHole $ Identifier "found!"
incrIO (VInt i) = do
    putStrLn "second eq."
    return $ VInt (i+1)
incrIO v = do
    putStrLn "third eq."
    return v


-- 1. pattern
-- 2. template
-- 3. param
-- 1 ~ 2 ~ 3
gParseAndAply :: String -> String -> String -> IO () -- String
gParseAndAply pattern template param = do
    pattern'  <- parseIO (parse <* eof) pattern
    -- template' <- parseIO (D <$> (parse <* eof)) template
    template' <- parseIO (parse <* eof) template
    param'    <- parseIO (parse <* eof) param
    (x, logs) <- runWriterT $ gapplyDeep "foo"
                                    pattern'
                                    template'
                                    param'
    -- ppPrint x
    mapM_ print logs
    putStrLn ""
    print $ pretty (x :: Expr)
    -- return $ show $ pretty x



-- matchTest :: String -> String -> IO ()
-- matchTest p a = do
--     xp :: Expr <- parseIO (parse <* eof) p
--     xa :: Expr <- parseIO (parse <* eof) a
--     -- binds <- execStateT (runErrorT (gmatch (mkG xp) (mkG xa))) def
--     binds <- runErrorT (execStateT (execStateT (match xp xa) def) def)
--     case binds of
--         Left err -> error err
--         Right (r,_) -> mapM_ (\ (nm,GNode _ x) -> putStrLn (nm ++ ": " ++ show (pretty x)) ) (M.toList r)


promoteTest :: Expr -> IO ()
promoteTest x = do
    putStrLn " [ ==================== ]"
    mapM_ (\ i -> putStrLn $ padRight ' ' 20 (show i) ++ showG i) $ universe $ mkG x
    putStrLn " [ ==================== ]"
    mapM_ (\ i -> putStrLn $ padRight ' ' 20 (show i) ++ showG i) $ universe $ mkG $ deepPromote x
    putStrLn " [ ==================== ]"


errorArbitrary :: a
errorArbitrary = error "in Arbitrary"


--------------------------------------------------------------------------------
-- Spec ------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Spec
    = Spec { language    :: String
           , version     :: [Int]
           , topLevels   :: [Either Binding Where]
           , objective   :: Maybe Objective
           , constraints :: [Expr]
           , metadata    :: [Metadata]
           }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Default Spec where
    def = Spec def def def def def def

instance NodeTag Spec

instance Hole Spec

instance GPlate Spec where
    gplate Spec{..} =
        (  map mkG topLevels
        ++ map mkG (maybeToList objective)
        ++ map mkG constraints
        , \ xs -> let
            l1 = length topLevels
            l2 = length (maybeToList objective)
            l3 = length constraints
            topLevels'   = fromGs $ take l1 xs
            objectives'  = fromGs $ take l2 $ drop l1 xs
            constraints' = fromGs $ take l3 $ drop l2 $ drop l1 xs
            in if l1 == length topLevels'  &&
                  l2 == length objectives' &&
                  l3 == length constraints'
                  then Spec language version topLevels'
                                             (listToMaybe objectives')
                                             constraints'
                                             metadata
                  else gplateError "Spec"
        )

instance MatchBind Spec

instance ParsePrint Spec where
    parse = do
        whiteSpace
        (lang,ver) <- pLanguage
        topLevels  <- parse
        obj        <- optionMaybe parse
        cons       <- pConstraints
        eof
        return (Spec lang ver topLevels obj cons [])
        where
            pLanguage :: Parser (String,[Int])
            pLanguage = do
                l  <- reserved "language" *> identifier
                is <- sepBy1 integer dot
                return (l, map fromInteger is)

            pConstraints :: Parser [Expr]
            pConstraints = choiceTry [ do reserved "such"; reserved "that"; sepEndBy parse comma
                                     , return []
                                     ]
    pretty (Spec{..}) = Pr.vcat
        $  ("language" <+> text language <+> Pr.hcat (intersperse Pr.dot (map Pr.int version)))
        :  map pretty topLevels
        ++ map pretty (maybeToList objective)
        ++ case constraints of [] -> []
                               _  -> "such that" : ( mapButLast (<> Pr.comma)
                                                   $ map (\ x -> Pr.nest 4 $ case x of Q q -> pretty q
                                                                                       _   -> pretty x )
                                                     constraints
                                                   )



newtype Where = Where Expr
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Where

instance Hole Where

instance GPlate Where where
    gplate (Where x) = gplateSingle Where x

instance MatchBind Where

instance ParsePrint Where where
    parse = error "do not use this one directly. use it via (parse :: [Where])"
    pretty (Where x) = "where" <+> pretty x

instance ParsePrint [Where] where
    parse = do
        let one = do reserved "where"
                     map Where <$> parse `sepBy1` comma
        concat <$> many1 one
        <?> "where statement"
    pretty = Pr.vcat . map pretty



data Binding
    = Find          Identifier Domain
    | Given         Identifier Domain
    | LettingType   Identifier Type
    | GivenType     Identifier Type
    | LettingDomain Identifier Domain
    | LettingExpr   Identifier Expr
    | LettingLambda Identifier Lambda
    | LettingQuan   Identifier QuantifierDecl
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Binding

instance Hole Binding

instance GPlate Binding where
    gplate (Find          i j) = gplateTwo Find          i j
    gplate (Given         i j) = gplateTwo Given         i j
    gplate (LettingType   i j) = gplateTwo LettingType   i j
    gplate (GivenType     i j) = gplateTwo GivenType     i j
    gplate (LettingDomain i j) = gplateTwo LettingDomain i j
    gplate (LettingExpr   i j) = gplateTwo LettingExpr   i j
    gplate (LettingLambda i j) = gplateTwo LettingLambda i j
    gplate (LettingQuan   i j) = gplateTwo LettingQuan  i j

instance MatchBind Binding

instance ParsePrint Binding where
    parse = error "do not use this one directly. use it via (parse :: [Binding])"
    pretty (Find          i j) = "find"    <+> pretty i <> Pr.colon <+> pretty j
    pretty (Given         i j) = "given"   <+> pretty i <> Pr.colon <+> pretty j
    pretty (LettingType   i j) = "letting" <+> pretty i <+> "be new type" <+> pretty j
    pretty (GivenType     i j) = "given"   <+> pretty i <> Pr.colon <+> "new type" <+> pretty j
    pretty (LettingDomain i j) = "letting" <+> pretty i <+> "be" <+> "domain" <+> pretty j
    pretty (LettingExpr   i j) = "letting" <+> pretty i <+> "be"              <+> pretty j
    pretty (LettingLambda i j) = "letting" <+> pretty i <+> "be" <+> "lambda" <+> pretty j
    pretty (LettingQuan   i j) = "letting" <+> pretty i <+> "be"              <+> pretty j

instance ParsePrint [Binding] where
    parse = do
        let one = choiceTry
                    [ do
                        reserved "find"
                        is <- parse `sepBy1` comma
                        colon
                        j <- parse
                        return [ Find i j | i <- is ]
                        <?> "find statement"
                    , do
                        reserved "given"
                        is <- parse `sepBy1` comma
                        colon
                        j <- parse
                        return [ Given i j | i <- is ]
                        <?> "given statement"
                    , do
                        reserved "letting"
                        is <- parse `sepBy1` comma
                        reserved "be"
                        reserved "new"
                        reserved "type"
                        j <- parse
                        case j of
                            TEnum {}    -> return ()
                            TUnnamed {} -> return ()
                            _           -> fail ""
                        return [ LettingType i j | i <- is ]
                        <?> "letting statement"
                    , do
                        reserved "given"
                        is <- parse `sepBy1` comma
                        colon
                        reserved "new"
                        reserved "type"
                        reserved "enum"
                        return [ GivenType i (TEnum Nothing) | i <- is ]
                        <?> "given statement"
                    , do
                        reserved "letting"
                        is <- parse `sepBy1` comma
                        reserved "be"
                        reserved "domain"
                        j <- parse
                        return [ LettingDomain i j | i <- is ]
                        <?> "letting statement"
                    , do
                        reserved "letting"
                        is <- parse `sepBy1` comma
                        reserved "be"
                        j <- parse
                        return [ LettingExpr i j | i <- is ]
                        <?> "letting statement"
                    , do
                        reserved "letting"
                        is <- parse `sepBy1` comma
                        reserved "be"
                        reserved "lambda"
                        j <- parse
                        return [ LettingLambda i j | i <- is ]
                        <?> "letting statement"
                    , do
                        reserved "letting"
                        is <- parse `sepBy1` comma
                        reserved "be"
                        j <- parse
                        return [ LettingQuan i j | i <- is ]
                        <?> "letting statement"
                    ]
        concat <$> many1 one
    pretty = Pr.vcat . map pretty

instance ParsePrint [Either Binding Where] where
    parse = concatMap (\ t -> case t of Left  bs -> map Left  bs
                                        Right ws -> map Right ws
                      ) <$> many parse
    pretty = Pr.vcat . map pretty



data Objective = OMin Expr | OMax Expr
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Objective

instance Hole Objective

instance GPlate Objective where
    gplate (OMin x) = gplateSingle OMin x
    gplate (OMax x) = gplateSingle OMax x

instance MatchBind Objective

instance ParsePrint Objective where
    parse = choiceTry [ OMin <$> (reserved "minimising" *> parse)
                      , OMin <$> (reserved "minimizing" *> parse)
                      , OMax <$> (reserved "maximising" *> parse)
                      , OMax <$> (reserved "maximizing" *> parse)
                      ]
    pretty (OMin x) = "minimising" <+> pretty x
    pretty (OMax x) = "maximising" <+> pretty x



data Metadata = Metadata
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Metadata

instance Hole Metadata

instance GPlate Metadata where
    gplate p@Metadata = gplateLeaf p

instance MatchBind  Metadata

instance ParsePrint Metadata where
    parse = undefined
    pretty = undefined



--------------------------------------------------------------------------------
-- Expr ------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Expr = EHole Identifier
    | V Value
    | D Domain
    | Q QuantifiedExpr
    | Bubble Expr Expr [Either Binding Where]
    | EOp Op [Expr]
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

isAtomicExpr :: Expr -> Bool
isAtomicExpr (EHole {}) = True
isAtomicExpr (V     {}) = True
isAtomicExpr (D     {}) = True
isAtomicExpr _          = False

instance IsString Expr where
    fromString = EHole . fromString

instance NodeTag Expr

instance Hole Expr where
    hole (EHole (Identifier "_")) = UnnamedHole
    hole (EHole (Identifier nm) ) = NamedHole nm
    hole _                        = NotAHole

instance GPlate Expr where
    gplate p@(EHole {}) = gplateLeaf p
    gplate (V x) = gplateSingle V x
    gplate (D x) = gplateSingle D x
    gplate (Q x) = gplateSingle Q x
    gplate (Bubble x y bs) =
        ( mkG x : mkG y : map mkG bs
        , \ xs -> let x's = fromGs $ take 1 xs
                      y's = fromGs $ take 1 $ drop 1 xs
                      bs' = fromGs $ drop 2 xs
                  in  case (x's,y's) of
                          ([x'],[y']) -> if equalLengths bs bs'
                                             then Bubble x' y' bs'
                                             else gplateError "Expr.Bubble[1]"
                          _ -> gplateError "Expr.Bubble[2]"
        )
    gplate (EOp op xs) =
        ( mkG op : map mkG xs
        , \ ys -> let op' = fromGs $ take 1 ys
                      xs' = fromGs $ drop 1 ys
                  in  if equalLengths [()] op' && equalLengths xs xs'
                          then EOp (head op') xs'
                          else gplateError "Expr.EOp"
        )

instance MatchBind Expr

instance ParsePrint Expr where
    parse = buildExpressionParser table (core <?> "expression")
        where
            postfixes :: Parser (Expr -> Expr)
            postfixes = do
                let allP = flip mapMaybe allValues $ \ op -> case opDescriptor op of
                            OpPostfix pa _ -> Just pa
                            _ -> Nothing
                let pImage = do
                        ys <- parens (parse `sepBy1` comma)
                        return $ \ x -> case ys of
                            [y] -> EOp Image [x,y]
                            _   -> EOp Image [x,V (VTuple ys)]
                let pPreImage = do
                        reservedOp "'"
                        ys <- parens (parse `sepBy1` comma)
                        return $ \ x -> case ys of
                            [y] -> EOp PreImage [x,y]
                            _   -> EOp PreImage [x,V (VTuple ys)]
                fs <- many1 (choiceTry $ pImage : pPreImage : allP)
                return $ foldr1 (.) (reverse fs)

            prefixes :: [Parser (Expr -> Expr)]
            prefixes = flip mapMaybe allValues $ \ op -> case opDescriptor op of
                OpPrefix pa _ -> Just pa
                _ -> Nothing

            infixes :: [(Int, Operator String () Identity Expr)]
            infixes = flip mapMaybe allValues $ \ op -> case opDescriptor op of
                OpInfix pa _ -> Just pa
                _ -> Nothing

            pLispyAndSpecial :: [Parser Expr]
            pLispyAndSpecial = flip mapMaybe allValues $ \ op -> case opDescriptor op of
                OpLispy   pa _ -> Just pa
                OpSpecial pa _ -> Just pa
                _ -> Nothing

            table :: OperatorTable String () Identity Expr
            table = [ Postfix postfixes ]
                  : map Prefix prefixes
                  : ( map (map snd)
                    . groupBy ((==) `on` fst)
                    . sortBy  (comparing fst)
                    ) infixes

            core :: Parser Expr
            core = choiceTry $ [ pBubble
                               , Q     <$> parse
                               , EHole <$> parse
                               , D     <$> parse       -- ordering is important: try: tuple (_) of (a,b,c)
                               , V     <$> parse
                               ] ++ pLispyAndSpecial
                                 ++ [ parens parse ]

            pBubble :: Parser Expr
            pBubble = parens $ do
                x  <- parse
                reservedOp "@"
                bs <- optionMaybe $ braces parse
                y  <- parse
                return $ Bubble x y (fromMaybe [] bs)

    pretty (EHole x) = pretty x
    pretty (V     x) = pretty x
    pretty (D     x) = pretty x
    pretty (Q     x) = Pr.parens (pretty x)
    pretty (Bubble x y bs) = Pr.parens (pretty x <+> "@" <+> bsPretty <+> pretty y)
        where
            bsPretty = case bs of [] -> Pr.empty
                                  _  -> Pr.braces $ Pr.vcat (map pretty bs)
    pretty p@(EOp {}) = prettyOp 10000 p
        where
            prettyOp :: Int -> Expr -> Pr.Doc
            -- prettyOp i x | trace msg False = undefined
            --     where msg = " -- prettyOp " ++ show i ++ " " ++ show x
            prettyOp _ param@(EOp Index   _) = let OpPostfix _ pr = opDescriptor Index   in pr param
            prettyOp _ param@(EOp Replace _) = let OpPostfix _ pr = opDescriptor Replace in pr param
            prettyOp envPrec param@(EOp op xs) = case (opDescriptor op, xs) of
                (OpLispy   _ pr, _    ) -> pr xs
                (OpInfix   _ pr, [i,j]) -> pr prettyOp envPrec i j
                (OpPrefix  _ pr, [i]  ) -> pr i
                (OpPostfix _ pr, [i]  ) -> pr i
                (OpSpecial _ pr, _    ) -> pr param
                _ -> error $ "prettyOp: " ++ show param
            prettyOp _ x = pretty x

instance Arbitrary Expr where
    arbitrary = deepPromote <$> oneof
        [ EHole <$> arbitrary
        , V     <$> arbitrary
        , D     <$> arbitrary
        ]
    shrink (V x) = map V $ shrink x
    shrink (D x) = map D $ shrink x
    shrink (EOp op xs) = map (EOp op) $ mapM shrink xs
    shrink _     = []



--------------------------------------------------------------------------------
-- Identifier ------------------------------------------------------------------
--------------------------------------------------------------------------------

newtype Identifier = Identifier String
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance IsString Identifier where
    fromString = Identifier

instance NodeTag Identifier

instance Hole Identifier where
    -- hole (Identifier nm) = NamedHole nm

instance GPlate Identifier

instance MatchBind Identifier

instance ParsePrint Identifier where
    parse = Identifier <$> identifier
    pretty (Identifier nm) = text nm

instance Arbitrary Identifier where
    arbitrary = Identifier . return <$> choose ('a', 'z')



data ComplexIdentifier
    = I Identifier
    | ITuple  [ComplexIdentifier]
    | IMatrix [ComplexIdentifier]
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag ComplexIdentifier

instance Hole ComplexIdentifier

instance GPlate ComplexIdentifier

instance MatchBind ComplexIdentifier

instance ParsePrint ComplexIdentifier where
    parse = choiceTry
        [ ITuple  <$> parens   (countSepAtLeast 2 parse comma)
        , IMatrix <$> brackets (countSepAtLeast 2 parse comma)
        , I <$> parse
        ]
    pretty (I        i) = pretty i
    pretty (ITuple  is) = prettyList Pr.parens   Pr.comma is
    pretty (IMatrix is) = prettyList Pr.brackets Pr.comma is

instance Arbitrary ComplexIdentifier where
    arbitrary = oneof
        [ I       <$> arbitrary
        , do (i,j,ks) <- arbitrary; return $ ITuple  $ i:j:ks
        , do (i,j,ks) <- arbitrary; return $ IMatrix $ i:j:ks
        ]



--------------------------------------------------------------------------------
-- Lambda ----------------------------------------------------------------------
--------------------------------------------------------------------------------

data Lambda = Lambda [(String, Type)] Expr
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Lambda

instance Hole Lambda

instance GPlate Lambda -- everything is a leaf!

instance MatchBind Lambda

instance ParsePrint Lambda where
    parse = braces $ do
        args <- sepBy1 ((,) <$> identifier <*> (colon *> parse)) comma
        reservedOp "->"
        x <- parse
        return $ Lambda args x
    pretty (Lambda args x) = Pr.braces (prettyListDoc id Pr.comma argsDoc <+> "->" <+> pretty x)
        where argsDoc = map (\ (i,t) -> text i <> Pr.colon <+> pretty t ) args



--------------------------------------------------------------------------------
-- QuantifierDecl --------------------------------------------------------------
--------------------------------------------------------------------------------

data QuantifierDecl = QuantifierDecl Lambda Lambda Expr
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag QuantifierDecl

instance Hole QuantifierDecl

instance GPlate QuantifierDecl -- everything is a leaf!

instance MatchBind QuantifierDecl

instance ParsePrint QuantifierDecl where
    parse = do
        reserved "quantifier"
        braces $ QuantifierDecl
            <$> (reserved "append"   >> parse)
            <*> (reserved "guard"    >> parse)
            <*> (reserved "identity" >> parse)
    pretty (QuantifierDecl app gua ide) =
        "quantifier" Pr.$$
        Pr.braces (
            Pr.nest 4 ("append  " <+> pretty app) Pr.$$
            Pr.nest 4 ("guard   " <+> pretty gua) Pr.$$
            Pr.nest 4 ("identity" <+> pretty ide)
        )



--------------------------------------------------------------------------------
-- QuantifiedExpr --------------------------------------------------------------
--------------------------------------------------------------------------------

data QuantifiedExpr = QuantifiedExpr
    { quanName     :: Identifier
    , quanVar      :: Identifier
    , quanOverDom  :: Maybe Domain
    , quanOverExpr :: Maybe (Op, Expr)
    , quanGuards   :: [Expr]
    , quanBody     :: Expr
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag QuantifiedExpr

instance Hole QuantifiedExpr

instance GPlate QuantifiedExpr where
    gplate (QuantifiedExpr qnName qnVar Nothing Nothing qnGuards qnBody) =
        ( [ mkG (EHole qnName), mkG (EHole qnVar), mkG qnBody ] ++ map mkG qnGuards
        , \ xs -> let qnName'   = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 xs
                      qnVar'    = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 1 xs
                      qnBody'   = fromGs $ take 1 $ drop 2 xs
                      qnGuards' = fromGs $          drop 3 xs
                  in  if length qnName'   == 1 &&
                         length qnVar'    == 1 &&
                         length qnBody'   == 1 &&
                         length qnGuards' == length qnGuards
                         then QuantifiedExpr (head qnName')
                                             (head qnVar')
                                             Nothing
                                             Nothing
                                             qnGuards'
                                             (head qnBody')
                         else gplateError "QuantifiedExpr[1]"
        )
    gplate (QuantifiedExpr qnName qnVar Nothing (Just (qnOp, qnExpr)) qnGuards qnBody) =
        ( [ mkG (EHole qnName), mkG (EHole qnVar), mkG qnBody, mkG qnOp, mkG qnExpr ] ++ map mkG qnGuards
        , \ xs -> let qnName'   = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 xs
                      qnVar'    = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 1 xs
                      qnBody'   = fromGs $ take 1 $ drop 2 xs
                      qnOp'     = fromGs $ take 1 $ drop 3 xs
                      qnExpr'   = fromGs $ take 1 $ drop 4 xs
                      qnGuards' = fromGs $          drop 5 xs
                  in  if length qnName'   == 1 &&
                         length qnVar'    == 1 &&
                         length qnBody'   == 1 &&
                         length qnOp'     == 1 &&
                         length qnExpr'   == 1 &&
                         length qnGuards' == length qnGuards
                         then QuantifiedExpr (head qnName')
                                             (head qnVar')
                                             Nothing
                                             (Just (head qnOp', head qnExpr'))
                                             qnGuards'
                                             (head qnBody')
                         else gplateError "QuantifiedExpr[2]"
        )
    gplate (QuantifiedExpr qnName qnVar (Just qnDom) Nothing qnGuards qnBody) =
        ( [ mkG (EHole qnName), mkG (EHole qnVar), mkG qnBody, mkG qnDom ] ++ map mkG qnGuards
        , \ xs -> let qnName'   = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 xs
                      qnVar'    = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 1 xs
                      qnBody'   = fromGs $ take 1 $ drop 2 xs
                      -- qnDom'    = mapMaybe (\ j -> case j of D     i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 3 xs
                      qnDom'    = fromGs $ take 1 $ drop 3 xs
                      qnGuards' = fromGs $          drop 4 xs
                  in  if length qnName'   == 1 &&
                         length qnVar'    == 1 &&
                         length qnBody'   == 1 &&
                         length qnDom'    == 1 &&
                         length qnGuards' == length qnGuards
                         then QuantifiedExpr (head qnName')
                                             (head qnVar')
                                             (Just (head qnDom'))
                                             Nothing
                                             qnGuards'
                                             (head qnBody')
                         else gplateError "QuantifiedExpr[3]"
        )
    gplate (QuantifiedExpr qnName qnVar (Just qnDom) (Just (qnOp, qnExpr)) qnGuards qnBody) =
        ( [ mkG (EHole qnName), mkG (EHole qnVar), mkG qnBody, mkG qnDom, mkG qnOp, mkG qnExpr ] ++ map mkG qnGuards
        , \ xs -> let qnName'   = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 xs
                      qnVar'    = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 1 xs
                      qnBody'   = fromGs $ take 1 $ drop 2 xs
                      -- qnDom'    = mapMaybe (\ j -> case j of D     i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 3 xs
                      qnDom'    = fromGs $ take 1 $ drop 3 xs
                      qnOp'     = fromGs $ take 1 $ drop 4 xs
                      qnExpr'   = fromGs $ take 1 $ drop 5 xs
                      qnGuards' = fromGs $          drop 6 xs
                  in  if length qnName'   == 1 &&
                         length qnVar'    == 1 &&
                         length qnBody'   == 1 &&
                         length qnDom'    == 1 &&
                         length qnOp'     == 1 &&
                         length qnExpr'   == 1 &&
                         length qnGuards' == length qnGuards
                         then QuantifiedExpr (head qnName')
                                             (head qnVar')
                                             (Just (head qnDom'))
                                             (Just (head qnOp', head qnExpr'))
                                             qnGuards'
                                             (head qnBody')
                         else gplateError "QuantifiedExpr[4]"
        )

instance MatchBind QuantifiedExpr

instance ParsePrint QuantifiedExpr where
    parse = do
        qnName   <- parse
        qnVars   <- parse `sepBy1` comma
        qnDom    <- optionMaybe (colon *> parse)
        qnExpr   <- optionMaybe ((,) <$> parse <*> parse)
        qnGuards <- optionMaybe (comma *> parse)
        qnBody   <- dot *> parse
        let
            f []     = error "The Impossible has happenned. in QuantifiedExpr.parse.f"
            f [i]    = QuantifiedExpr qnName i qnDom qnExpr (maybeToList qnGuards) qnBody
            f (i:is) = QuantifiedExpr qnName i qnDom qnExpr [] (Q $ f is)
        return (f qnVars)
    pretty (QuantifiedExpr qnName qnVar qnDom qnExpr qnGuards qnBody)
        =   pretty qnName
        <+> pretty qnVar
        <+> ( case qnDom of
                Nothing -> Pr.empty
                Just i  -> Pr.colon  <+> pretty i
            )
        <+> ( case qnExpr of
                Nothing     -> Pr.empty
                Just (op,i) -> pretty op <+> pretty i
            )
        <+> ( case qnGuards of
                []  -> Pr.empty
                [i] -> Pr.comma <+> pretty i
                _   -> error "Multiple guards, what the hell?"
            )
        <+> Pr.dot
        <+> pretty qnBody



--------------------------------------------------------------------------------
-- Op --------------------------------------------------------------------------
--------------------------------------------------------------------------------

-- the data type for operators in Essence
data Op
    = Plus | Minus | Times | Div | Mod | Pow | Abs | Negate
    | Factorial
    | Lt | Leq | Gt | Geq | Neq | Eq
    | Not | Or | And | Imply | Iff
    | Union | Intersect | Subset | SubsetEq | Supset | SupsetEq
    | Card | Elem | Max | Min
    | ToSet | ToMSet | ToRel | Defined | Range
    | Image | PreImage | Inverse
    | Together | Apart
    | Party | Participants | Parts
    | Freq | Hist

    | Index

    | HasType | HasDomain

    | Replace

    | AllDiff

    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)

commutativeOps :: S.Set Op
commutativeOps = S.fromList
    [ Plus, Times
    , Neq, Eq
    , Or, And, Iff
    , Union, Intersect
    ]

associativeOps :: S.Set Op
associativeOps = S.fromList
    [ Plus
    ]

opFace :: Op -> String
opFace Plus         = "+"
opFace Minus        = "-"
opFace Times        = "*"
opFace Div          = "/"
opFace Mod          = "%"
opFace Pow          = "^"
opFace Abs          = "abs"
opFace Negate       = "-"
opFace Factorial    = "!"
opFace Lt           = "<"
opFace Leq          = "<="
opFace Gt           = ">"
opFace Geq          = ">="
opFace Neq          = "!="
opFace Eq           = "="
opFace Not          = "!"
opFace Or           = "\\/"
opFace And          = "/\\"
opFace Imply        = "=>"
opFace Iff          = "<=>"
opFace Union        = "union"
opFace Intersect    = "intersect"
opFace Subset       = "subset"
opFace SubsetEq     = "subseteq"
opFace Supset       = "supset"
opFace SupsetEq     = "supseteq"
opFace Card         = "card"
opFace Elem         = "in"
opFace Max          = "max"
opFace Min          = "min"
opFace ToSet        = "toSet"
opFace ToMSet       = "toMSet"
opFace ToRel        = "toRel"
opFace Defined      = "defined"
opFace Range        = "range"
opFace Image        = "image"
opFace PreImage     = "preimage"
opFace Inverse      = "inverse"
opFace Together     = "together"
opFace Apart        = "apart"
opFace Party        = "party"
opFace Participants = "participants"
opFace Parts        = "parts"
opFace Freq         = "freq"
opFace Hist         = "hist"
opFace Index        = ""
opFace HasType      = "::"
opFace HasDomain    = ":"
opFace Replace      = ""
opFace AllDiff      = "alldifferent"

instance NodeTag Op

instance Hole Op

instance GPlate Op where
    gplate = gplateLeaf

instance MatchBind Op

instance ParsePrint Op where
    isoParsePrint = fromPairs $ mapMaybe (\ v ->
                                            case opFace v of
                                                ""   -> Nothing
                                                face -> Just (v, face)
                                         ) allValues

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
                    i <- parse
                    reservedOp "->"
                    j <- parse
                    return $ \ x -> EOp Replace [x,i,j]
                pr (EOp Replace [a,b,c]) =
                    (if isAtomicExpr a then id else Pr.parens) (pretty a)
                    <+> Pr.braces (pretty b <+> "->" <+> pretty c)
                pr x = error $ "pretty Replace: " ++ show x
        helper op@AllDiff = genLispy op 1



--------------------------------------------------------------------------------
-- Value -----------------------------------------------------------------------
--------------------------------------------------------------------------------

data Value = VHole Identifier
    | VBool   Bool
    | VInt   Integer
    | VEnum  Identifier Type
    | VMatrix    [Expr]         -- uniform type.
    | VTuple     [Expr]
    | VSet       [Expr]         -- uniform type. unique.
    | VMSet      [Expr]         -- uniform type.
    | VFunction  [Expr]         -- VTuple#2. uniform type.
    | VRelation  [Expr]         -- VTuple. uniform type.
    | VPartition [Expr]         -- VSet. uniform type.
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Value

instance Hole Value where
    hole (VHole (Identifier "_")) = UnnamedHole
    hole (VHole (Identifier nm) ) = NamedHole nm
    hole _                        = NotAHole

instance GPlate Value where
    gplate p@(VHole {}) = gplateLeaf p
    gplate p@(VBool {}) = gplateLeaf p
    gplate p@(VInt  {}) = gplateLeaf p
    gplate (VEnum i t)  = ( [mkG i, mkG t]
                          , \ xs -> case xs of
                              [i',t'] -> case (fromG i', fromG t') of
                                  (Just i'', Just t'') -> VEnum i'' t''
                                  _ -> gplateError "Value.VEnum[1]"
                              _ -> gplateError "Value.VEnum[2]"
                          )
    gplate   (VMatrix    xs) = gplateUniList VMatrix    xs
    gplate   (VTuple     xs) = gplateUniList VTuple     xs
    gplate   (VSet       xs) = gplateUniList VSet       xs
    gplate   (VMSet      xs) = gplateUniList VMSet      xs
    gplate   (VFunction  xs) = gplateUniList VFunction  xs
    gplate   (VRelation  xs) = gplateUniList VRelation  xs
    gplate   (VPartition xs) = gplateUniList VPartition xs

instance MatchBind Value

instance ParsePrint Value where
    parse = choiceTry
                [ pHole, pBool, pInt
                , pMatrix, pTuple, pSet, pMSet
                , pFunction, pRelation, pPartition
                ] <?> "value"
        where
            pHole = VHole <$> parse

            pBool = VBool False <$ reserved "false"
                    <|>
                    VBool True  <$ reserved "true"

            pInt = VInt <$> integer

            pMatrix = VMatrix <$> brackets (sepBy parse comma)

            pTuple = try (do reserved "tuple"; VTuple <$> parens (sepBy parse comma))
                     <|>
                     VTuple <$> parens (countSepAtLeast 2 parse comma)

            pSet = do reserved "set"; VSet <$> braces (sepBy parse comma)

            pMSet = do reserved "mset"; VMSet <$> braces (sepBy parse comma)

            pFunction = do reserved "function"; VFunction <$> braces (sepBy pTuple2 comma)
                where
                    pTuple2 :: Parser Expr
                    pTuple2 = do
                        i <- parse
                        reservedOp "->"
                        j <- parse
                        return $ V (VTuple [i,j])

            pRelation = do reserved "relation"; VRelation <$> braces (sepBy (V <$> pTuple) comma)

            pPartition = do reserved "partition"; VPartition <$> braces (sepBy aPart comma)
                where
                    aPart :: Parser Expr
                    aPart = (V . VSet) <$> braces (sepBy parse comma)

    pretty (VHole (Identifier nm)) = text nm
    pretty (VBool False) = "false"
    pretty (VBool True ) = "true"
    pretty (VInt  i    ) = Pr.integer i
    pretty (VEnum i _  ) = pretty i
    pretty (VMatrix xs) = prettyList Pr.brackets Pr.comma xs
    pretty (VTuple [] ) = "tuple ()"
    pretty (VTuple [x]) = "tuple" <+> Pr.parens (pretty x)
    pretty (VTuple xs ) = prettyList Pr.parens Pr.comma xs
    pretty (VSet  xs) = "set" <+> prettyList Pr.braces Pr.comma xs
    pretty (VMSet xs) = "mset" <+> prettyList Pr.braces Pr.comma xs
    pretty (VFunction xs) = "function" <+> prettyListDoc Pr.braces Pr.comma (map prE xs)
        where
            prE (V (VTuple [i,j])) = pretty i <+> "->" <+> pretty j
            prE p = pretty p
    pretty (VRelation  xs) = "relation"  <+> prettyList Pr.braces Pr.comma xs
    pretty (VPartition xs) = "partition" <+> prettyListDoc Pr.braces Pr.comma (map prE xs)
        where
            prE (V (VSet vs)) = prettyList Pr.braces Pr.comma vs
            prE p = pretty p

instance Arbitrary Value where
    arbitrary = deepPromote . VHole <$> arbitrary



--------------------------------------------------------------------------------
-- Domain ----------------------------------------------------------------------
--------------------------------------------------------------------------------

data Domain = DHole Identifier
    | DBool
    | DInt                (Range Expr)
    | DEnum    Identifier (Range Identifier)
    | DUnnamed Identifier
    | DMatrix  Domain Domain
    | AnyDom { dConstr  :: AnyDomEnum
             , dElement :: [Domain]
             , dAttrs   :: DomainAttrs
             }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Domain

instance Hole Domain where
    hole (DHole (Identifier "_")) = UnnamedHole
    hole (DHole (Identifier nm) ) = NamedHole nm
    hole _                        = NotAHole

instance GPlate Domain where
    gplate p@(DHole {}) = gplateLeaf p
    gplate p@(DBool {}) = gplateLeaf p
    gplate (DInt x) = gplateSingle DInt x
    gplate (DEnum nm x) =
        ( [mkG nm, mkG x]
        , \ xs ->
            case xs of
                [mnm,mx] ->
                    case (fromG mnm, fromG mx) of
                        (Just nm', Just x') -> DEnum nm' x'
                        _                   -> gplateError "Domain DEnum[1]"
                _ -> gplateError "Domain DEnum[2]"
        )
    gplate (DUnnamed x) = gplateSingle DUnnamed x
    gplate (DMatrix i e) = gplateUniList (\ [i',e'] -> DMatrix i' e' ) [i,e]
    gplate (AnyDom nm es as) =
        ( mkG nm : mkG as : map mkG es
        , \ xs -> let nm' = fromGs $ take 1 xs
                      as' = fromGs $ take 1 $ drop 1 xs
                      es' = fromGs $ drop 2 xs
                  in  if length nm' == 1 &&
                         length as' == 1 &&
                         length es' == length es
                          then AnyDom (head nm') es' (head as')
                          else gplateError "Domain AnyDom"
        )

instance MatchBind Domain

instance ParsePrint Domain where
    parse = choiceTry
                [ pBool, pInt, pEnum, pUnnamed, pMatrix
                , pTuple, pSetMSet "set" DSet, pSetMSet "mset" DMSet
                , pFunction, pRelation, pPartition
                , pDHole
                ]
        where
            pDHole = DHole <$> parse

            pBool = DBool <$ reserved "bool"

            pInt     = do reserved "int" ; DInt  <$>           (try (parens parse) <|> return RAll)

            pEnum    = do reserved "enum"; DEnum <$> parse <*> (try (parens parse) <|> return RAll)

            -- needed to disambiguate from DHole
            -- DHole can still be resolved to DUnnamed, after parsing.
            pUnnamed = do reserved "unnamed";  DUnnamed <$> parse

            pMatrix = do
                reserved "matrix"
                reserved "indexed"
                reserved "by"
                is <- brackets (parse `sepBy1` comma)
                reserved "of"
                e  <- parse
                return $ foldr DMatrix e is

            pTuple = do
                reserved "tuple"
                as <- parse
                reserved "of"
                es <- parens (parse `sepBy` comma)
                return $ AnyDom DTuple es as

            pSetMSet kw en = do
                reserved kw
                as <- parse
                reserved "of"
                e  <- parse
                return $ AnyDom en [e] as

            pFunction = do
                reserved "function"
                as <- parse
                fr <- parse
                reservedOp "->"
                to <- parse
                return $ AnyDom DFunction [fr,to] as

            pRelation = do
                reserved "relation"
                as <- parse
                reserved "of"
                es <- parens (parse `sepBy` reservedOp "*")
                return $ AnyDom DRelation es as

            pPartition = do
                reserved "partition"
                as <- parse
                reserved "from"
                e  <- parse
                return $ AnyDom DPartition [e] as

    pretty (DHole (Identifier nm)) = text nm
    pretty DBool = "bool"
    pretty (DInt RAll) = "int"
    pretty (DInt r   ) = "int" <> Pr.parens (pretty r)
    pretty (DEnum i RAll) = "enum" <+> pretty i
    pretty (DEnum i r   ) = "enum" <+> pretty i <> Pr.parens (pretty r)
    pretty (DUnnamed i) = "unnamed" <+> pretty i
    pretty (DMatrix i e) = "matrix" <+> "indexed"
                       <+> "by" <+> prettyList Pr.brackets Pr.comma is
                       <+> "of" <+> pretty e'
        where
            (is,e') = helper i e
            helper a b = first (a:) $ case b of DMatrix c d -> helper c d
                                                _           -> ([], b)
    pretty (AnyDom DTuple es as) = "tuple" <+> pretty as <+> "of"
                                                <+> prettyList Pr.parens Pr.comma es
    pretty (AnyDom DSet  [e] as) = "set"  <+> pretty as <+> "of" <+> pretty e
    pretty (AnyDom DMSet [e] as) = "mset" <+> pretty as <+> "of" <+> pretty e
    pretty (AnyDom DFunction [fr,to] as) = "function"  <+> pretty as <+> pretty fr <+> "->" <+> pretty to
    pretty (AnyDom DRelation es as) = "relation" <+> pretty as <+> "of"
                                                      <+> prettyList Pr.parens "*" es
    pretty (AnyDom DPartition [e] as) = "partition" <+> pretty as <+> "from" <+> pretty e
    pretty p = error ("Invalid domain: " ++ show p)

instance Arbitrary Domain where
    arbitrary = deepPromote <$> oneof
        [ DHole    <$> arbitrary
        , return DBool
        , DInt     <$> arbitrary
        , DEnum    <$> arbitrary <*> arbitrary
        , DUnnamed <$> arbitrary
        , DMatrix  <$> arbitrary <*> arbitrary
        , AnyDom DTuple     <$> arbitrary              <*> arbitrary
        , AnyDom DSet       <$> (return <$> arbitrary) <*> arbitrary
        , AnyDom DMSet      <$> (return <$> arbitrary) <*> arbitrary
        , do (fr,to) <- arbitrary; AnyDom DFunction [fr,to] <$> arbitrary
        , AnyDom DRelation  <$> arbitrary              <*> arbitrary
        , AnyDom DPartition <$> (return <$> arbitrary) <*> arbitrary
        ]



data Range a = RAll | RList [a] | RFromTo (Maybe a) (Maybe a)
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance Data a => NodeTag (Range a)

instance Hole (Range a)

instance (Eq a, Show a, Data a, GPlate a) => GPlate (Range a) where
    gplate RAll = gplateLeaf RAll
    gplate (RList xs) = gplateUniList RList xs
    gplate p@(RFromTo Nothing  Nothing ) = gplateLeaf p
    gplate   (RFromTo Nothing  (Just y)) = gplateSingle  (\ y'      -> RFromTo Nothing   (Just y') ) y
    gplate   (RFromTo (Just x) Nothing ) = gplateSingle  (\ x'      -> RFromTo (Just x') Nothing   ) x
    gplate   (RFromTo (Just x) (Just y)) = gplateUniList (\ [x',y'] -> RFromTo (Just x') (Just y') ) [x,y]

instance MatchBind a => MatchBind (Range a)

instance ParsePrint a => ParsePrint (Range a) where
    parse = try pRList <|> pRFromTo
        where
            pRList = do
                i <- optionMaybe parse
                dot; dot
                j <- optionMaybe parse
                return $ RFromTo i j
            pRFromTo = RList <$> sepBy parse comma
    pretty RAll = error "do not call pretty Range.RAll"
    pretty (RList      xs) = prettyList id Pr.comma xs
    pretty (RFromTo fr to) = fr' <> ".." <> to'
        where
            fr' = fromMaybe Pr.empty (pretty <$> fr)
            to' = fromMaybe Pr.empty (pretty <$> to)

instance Arbitrary a => Arbitrary (Range a) where
    arbitrary = oneof
        [ return RAll
        , RList   <$> arbitrary
        , RFromTo <$> arbitrary <*> arbitrary
        ]



data AnyDomEnum = DTuple | DSet | DMSet | DFunction | DRelation | DPartition
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)

instance NodeTag AnyDomEnum

instance Hole AnyDomEnum

instance GPlate AnyDomEnum

instance MatchBind AnyDomEnum

instance ParsePrint AnyDomEnum where
    isoParsePrint = fromPairs
                        [ ( DTuple    , "tuple"     )
                        , ( DSet      , "set"       )
                        , ( DMSet     , "mset"      )
                        , ( DFunction , "function"  )
                        , ( DRelation , "set"       )
                        , ( DPartition, "partition" )
                        ]

instance Arbitrary AnyDomEnum where
    arbitrary = elements [minBound .. maxBound]



newtype DomainAttrs = DomainAttrs [DomainAttr]
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag DomainAttrs

instance Hole DomainAttrs

instance GPlate DomainAttrs where
    gplate (DomainAttrs xs) = gplateUniList DomainAttrs xs

instance MatchBind DomainAttrs where
    match p@(DomainAttrs ps) a@(DomainAttrs as) = do
        lift $ lift $ modify ((mkG p, mkG a) :) -- add this node on top of the call stack.
        helper (DontCare `elem` ps)
               (sort $ filter (/=DontCare) ps)
               (sort $ filter (/=DontCare) as)
        lift $ lift $ modify tail
        where
            checkMatch :: Monad m => DomainAttr -> DomainAttr -> StateT (M.Map String GNode) (StateT [(GNode,GNode)] m) Bool
            checkMatch i j = do
                res <- runErrorT (match i j)
                case res of
                    Right _ -> return True
                    _       -> return False

            tryMatch :: Monad m => DomainAttr -> [DomainAttr] -> StateT (M.Map String GNode) (StateT [(GNode,GNode)] m) (Bool, [DomainAttr])
            tryMatch _ []     = return (False, [])
            tryMatch i (j:js) = do
                b <- checkMatch i j
                if b
                    then return (b,js)
                    else second (j:) `liftM` tryMatch i js

            helper :: Monad m => Bool -> [DomainAttr] -> [DomainAttr] -> ErrorT Doc (StateT (M.Map String GNode) (StateT [(GNode,GNode)] m)) ()
            helper _    []     []     = return ()  -- if both attr lists are fully consumed.
            helper True []     _      = return ()  -- if the pattern list is fully consumed, we DontCare.
            helper d    (x:xs) ys = do
                (res, ys') <- lift $ tryMatch x ys
                if res
                    then helper d xs ys'
                    else throwError $ "attribute in pattern not found in actual: " <+> pretty x
            helper _ _ ys = throwError $ "some attibutes in actual not matched: " <+> prettyList id Pr.comma ys

instance ParsePrint DomainAttrs where
    parse = DomainAttrs . fromMaybe [] <$> optionMaybe (parens (parse `sepBy` comma))
    pretty (DomainAttrs []) = Pr.empty
    pretty (DomainAttrs xs) = prettyList Pr.parens Pr.comma xs

instance Arbitrary DomainAttrs where
    arbitrary = DomainAttrs <$> arbitrary



data DomainAttr
    = OnlyName DomainAttrEnum
    | NameValue DomainAttrEnum Expr
    | DontCare
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag DomainAttr

instance Hole DomainAttr

instance GPlate DomainAttr where
    gplate (OnlyName e) = gplateSingle OnlyName e
    gplate (NameValue e x) =
        ( [mkG e, mkG x]
        , \ ex ->
            case ex of
                [me,mx] ->
                    case (fromG me, fromG mx) of
                        (Just e', Just x') -> NameValue e' x'
                        _ -> gplateError "DomainAttr[1]"
                _ -> gplateError "DomainAttr[2]"
        )
    gplate p@(DontCare {}) = gplateLeaf p

instance MatchBind DomainAttr

instance ParsePrint DomainAttr where
    parse = choiceTry [pNameValue, pOnlyName, pDontCare]
        where
            pOnlyName  = OnlyName  <$> parse
            pNameValue = NameValue <$> parse <*> parse
            pDontCare  = DontCare  <$  reservedOp "_"
    pretty (OnlyName e) = pretty e
    pretty (NameValue e x) = pretty e <+> pretty x
    pretty DontCare = "_"

instance Arbitrary DomainAttr where
    arbitrary = oneof
        [ OnlyName  <$> arbitrary
        , NameValue <$> arbitrary <*> arbitrary
        , return DontCare
        ]



data DomainAttrEnum
    = AttrRepresentation
    | AttrSize
    | AttrMinSize
    | AttrMaxSize
    | AttrOccr
    | AttrMinOccr
    | AttrMaxOccr
    | AttrTotal
    | AttrPartial
    | AttrInjective
    | AttrSurjective
    | AttrBijective
    | AttrRegular
    | AttrComplete
    | AttrPartSize
    | AttrMinPartSize
    | AttrMaxPartSize
    | AttrNumParts
    | AttrMinNumParts
    | AttrMaxNumParts
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)

instance NodeTag DomainAttrEnum

instance Hole DomainAttrEnum

instance GPlate DomainAttrEnum

instance MatchBind DomainAttrEnum

instance ParsePrint DomainAttrEnum where
    isoParsePrint = fromPairs
            [ ( AttrRepresentation , "representation" )
            , ( AttrSize           , "size"           )
            , ( AttrMinSize        , "minSize"        )
            , ( AttrMaxSize        , "maxSize"        )
            , ( AttrOccr           , "occr"           )
            , ( AttrMinOccr        , "minOccr"        )
            , ( AttrMaxOccr        , "maxOccr"        )
            , ( AttrTotal          , "total"          )
            , ( AttrPartial        , "partial"        )
            , ( AttrInjective      , "injective"      )
            , ( AttrSurjective     , "surjective"     )
            , ( AttrBijective      , "bijective"      )
            , ( AttrRegular        , "regular"        )
            , ( AttrComplete       , "complete"       )
            , ( AttrPartSize       , "partSize"       )
            , ( AttrMinPartSize    , "minPartSize"    )
            , ( AttrMaxPartSize    , "maxPartSize"    )
            , ( AttrNumParts       , "numParts"       )
            , ( AttrMinNumParts    , "minNumParts"    )
            , ( AttrMaxNumParts    , "maxNumParts"    )
            ]

instance Arbitrary DomainAttrEnum where
    arbitrary = elements [minBound .. maxBound]



--------------------------------------------------------------------------------
-- Type ------------------------------------------------------------------------
--------------------------------------------------------------------------------

data Type = THole Identifier
    | TBool
    | TInt
    | TEnum (Maybe [Identifier])
    | TUnnamed Expr
    | TMatrix Type Type
    | TLambda [Type] Type
    | AnyType AnyTypeEnum [Type]
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Type

instance Hole Type where
    hole (THole (Identifier "_")) = UnnamedHole
    hole (THole (Identifier nm) ) = NamedHole nm
    hole _                        = NotAHole

instance GPlate Type where
    gplate p@(THole    {}) = gplateLeaf p
    gplate p@(TBool    {}) = gplateLeaf p
    gplate p@(TInt     {}) = gplateLeaf p
    gplate p@(TEnum    {}) = gplateLeaf p
    gplate p@(TUnnamed {}) = gplateLeaf p
    gplate (TMatrix i  e) = gplateUniList (\ [i',e']  -> TMatrix i' e'  ) [i,e]
    gplate (TLambda is o) = gplateUniList (\ (o':is') -> TLambda is' o' ) (o:is)
    gplate (AnyType e ts) = gplateUniList (AnyType e) ts

instance MatchBind Type

instance ParsePrint Type where
    parse = choiceTry
                [ pTHole, pTBool, pTInt, pEnum, pUnnamed, pMatrix
                , pTTuple, pTSet, pTMSet
                , pTFunction, pTRelation, pTPartition
                , pTLambda
                ]
        where
            pTHole  = THole <$> parse
            pTBool  = TBool <$  reserved "bool"
            pTInt   = TInt  <$  reserved "int"
            pEnum   = do reserved "enum" ; TEnum <$> optionMaybe (braces (sepBy parse comma))
            pUnnamed = do reserved "of"; reserved "size"; TUnnamed <$> parse
            pMatrix = do
                reserved "matrix"
                reserved "indexed"
                reserved "by"
                is <- brackets (parse `sepBy1` comma)
                reserved "of"
                e  <- parse
                return $ foldr TMatrix e is
            pTTuple = do reserved "tuple"; reserved "of"; AnyType TTuple <$> parens (sepBy parse comma)
            pTSet   = do reserved "set"  ; reserved "of"; AnyType TSet  . return <$> parse
            pTMSet  = do reserved "mset" ; reserved "of"; AnyType TMSet . return <$> parse
            pTFunction = do
                reserved "function"
                fr <- parse
                reservedOp "->"
                to <- parse
                return (AnyType TFunction [fr,to])
            pTRelation  = do reserved "relation" ; reserved "of"  ; AnyType TRelation  <$> parens (sepBy parse (reservedOp "*"))
            pTPartition = do reserved "partition"; reserved "from"; AnyType TPartition . return <$> parse
            pTLambda = do
                reserved "lambda"
                braces $ do
                    is <- sepBy1 parse comma
                    reservedOp "->"
                    o  <- parse
                    return (TLambda is o)

    pretty (THole i) = pretty i
    pretty TBool = "bool"
    pretty TInt  = "int"
    pretty (TEnum Nothing  ) = "enum"
    pretty (TEnum (Just xs)) = "enum" <+> prettyList Pr.braces Pr.comma xs
    pretty (TUnnamed i) = "of size" <+> pretty i
    pretty (TMatrix i e) = "matrix" <+> "indexed"
                       <+> "by" <+> prettyList Pr.brackets Pr.comma is
                       <+> "of" <+> pretty e'
        where
            (is,e') = helper i e
            helper a b = first (a:) $ case b of TMatrix c d -> helper c d
                                                _           -> ([], b)
    pretty (TLambda is  o) = "lambda" <+> Pr.braces (prettyList id Pr.comma is <+> "->" <+> pretty o)
    pretty (AnyType TTuple ts) = "tuple" <+> "of" <+> prettyList Pr.parens Pr.comma ts
    pretty (AnyType TSet  [t]) = "set"  <+> "of" <+> pretty t
    pretty (AnyType TMSet [t]) = "mset" <+> "of" <+> pretty t
    pretty (AnyType TFunction [fr,to]) = "function" <+> pretty fr <+> "->" <+> pretty to
    pretty (AnyType TRelation  ts ) = "relation"  <+> "of"   <+> prettyList Pr.parens "*" ts
    pretty (AnyType TPartition [t]) = "partition" <+> "from" <+> pretty t
    pretty p = error ("Invalid type: " ++ show p)

instance Arbitrary Type where
    arbitrary = oneof
        [ THole <$> arbitrary
        , return TBool
        , return TInt
        , TEnum <$> arbitrary
        , TMatrix <$> arbitrary <*> arbitrary
        , do (i,is,o) <- arbitrary; return $ TLambda (i:is) o
        , AnyType TTuple              <$> arbitrary
        , AnyType TSet  . return      <$> arbitrary
        , AnyType TMSet . return      <$> arbitrary
        , do (fr,to)  <- arbitrary; return $ AnyType TFunction [fr,to]
        , AnyType TRelation           <$> arbitrary
        , AnyType TPartition . return <$> arbitrary
        ]
    -- shrink (TLambda is o) = do
    --     is' <- shrink is
    --     o'  <- shrink o
    --     THole (Identifier "_") : o' : is' ++ map (\ t -> TLambda t o') (drop 1 $ take (length is) $ inits is')
    -- shrink (AnyType enum is) | enum `elem` [TTuple, TRelation] = do
    --     is' <- shrink is
    --     THole (Identifier "_") : is' ++ map (AnyType enum) (take (length is) $ inits is')
    -- shrink (AnyType enum ts) = do
    --     ts' <- shrink ts
    --     return $ AnyType enum ts'
    -- shrink _ = []



data AnyTypeEnum = TTuple | TSet | TMSet | TFunction | TRelation | TPartition
    deriving (Eq, Ord, Read, Show, Enum, Bounded, Data, Typeable, Generic)

instance NodeTag AnyTypeEnum

instance Hole AnyTypeEnum

instance GPlate AnyTypeEnum

instance MatchBind AnyTypeEnum

instance ParsePrint AnyTypeEnum where
    isoParsePrint = fromPairs
                        [ ( TTuple    , "tuple"     )
                        , ( TSet      , "set"       )
                        , ( TMSet     , "mset"      )
                        , ( TFunction , "function"  )
                        , ( TRelation , "set"       )
                        , ( TPartition, "partition" )
                        ]

instance Arbitrary AnyTypeEnum where
    arbitrary = elements [minBound .. maxBound]



--------------------------------------------------------------------------------
-- RuleRepr --------------------------------------------------------------------
--------------------------------------------------------------------------------

data RuleRepr = RuleRepr
    { reprFilename           :: String
    , reprName               :: String
    , reprTemplate           :: Domain
    , reprPrologueStructural :: Maybe Expr
    , reprLocals             :: [Either Binding Where]
    , reprCases              :: [RuleReprCase]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag RuleRepr

instance Hole RuleRepr

instance GPlate RuleRepr where
    gplate RuleRepr {..} =
        (  mkG reprTemplate
        :  map mkG (maybeToList reprPrologueStructural)
        ++ map mkG reprLocals
        ++ map mkG reprCases
        , \ xs -> let
            l1 = 1
            l2 = length (maybeToList reprPrologueStructural)
            l3 = length reprLocals
            l4 = length reprCases
            reprTemplate'           = fromGs $ take l1 xs
            reprPrologueStructural' = fromGs $ take l2 $ drop l1 xs
            reprLocals'             = fromGs $ take l3 $ drop l2 $ drop l1 xs
            reprCases'              = fromGs $ take l4 $ drop l3 $ drop l2 $ drop l1 xs
            in if l1 == length reprTemplate' &&
                  l2 == length reprPrologueStructural' &&
                  l3 == length reprLocals' &&
                  l4 == length reprCases'
                  then RuleRepr
                        reprFilename
                        reprName
                        (head reprTemplate')
                        (listToMaybe reprPrologueStructural')
                        reprLocals'
                        reprCases'
                  else gplateError "RuleRepr"
        )

instance MatchBind RuleRepr

instance ParsePrint RuleRepr where
    parse = do
        whiteSpace
        name   <- reservedOp "~~>" >> identifier
        templ  <- reservedOp "~~>" >> parse
        cons   <- optionMaybe (reservedOp "~~>" >> parse)
        locals <- parse
        cases  <- many1 parse
        return (RuleRepr "" name templ cons locals cases)
    pretty RuleRepr {..} = Pr.vcat $ [ "~~>" <+> text reprName
                                     , "~~>" <+> pretty reprTemplate ]
                                  ++ [ "~~>" <+> pretty s | Just s <- [reprPrologueStructural] ]
                                  ++ map (Pr.nest 4 . pretty) reprLocals
                                  ++ [ text "" ]
                                  ++ map pretty reprCases



data RuleReprCase = RuleReprCase
    { reprCasePattern    :: Domain
    , reprCaseStructural :: Maybe Expr
    , reprCaseLocals     :: [Either Binding Where]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag RuleReprCase

instance Hole RuleReprCase

instance GPlate RuleReprCase where
    gplate RuleReprCase {..} =
        (  mkG reprCasePattern
        :  map mkG (maybeToList reprCaseStructural)
        ++ map mkG reprCaseLocals
        , \ xs -> let
            l1 = 1
            l2 = length (maybeToList reprCaseStructural)
            l3 = length reprCaseLocals
            reprCasePattern'    = fromGs $ take l1 xs
            reprCaseStructural' = fromGs $ take l2 $ drop l1 xs
            reprCaseLocals'     = fromGs $ take l3 $ drop l2 $ drop l1 xs
            in if l1 == length reprCasePattern'  &&
                  l2 == length reprCaseStructural' &&
                  l3 == length reprCaseLocals'
                  then RuleReprCase
                        (head reprCasePattern')
                        (listToMaybe reprCaseStructural')
                        reprCaseLocals'
                  else gplateError "RuleReprCase"
        )

instance MatchBind RuleReprCase

instance ParsePrint RuleReprCase where
    parse = do
        pattern <- reservedOp "***" >> parse
        cons    <- optionMaybe (reservedOp "~~>" >> parse)
        locals  <- parse
        return (RuleReprCase pattern cons locals)
    pretty RuleReprCase {..} = Pr.vcat $ [ "***" <+> pretty reprCasePattern ]
                                      ++ [ "~~>" <+> pretty s | Just s <- [reprCaseStructural] ]
                                      ++ map (Pr.nest 4 . pretty) reprCaseLocals
                                      ++ [ text "" ]



--------------------------------------------------------------------------------
-- RuleRefn --------------------------------------------------------------------
--------------------------------------------------------------------------------

data RuleRefn = RuleRefn
    { refnLevel     :: Maybe Int
    , refnFilename  :: String
    , refnPattern   :: Expr
    , refnTemplates :: [Expr]
    , refnLocals    :: [Either Binding Where]
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag RuleRefn

instance Hole RuleRefn

instance GPlate RuleRefn where
    gplate RuleRefn {..} = 
        (  mkG refnPattern
        :  map mkG refnTemplates
        ++ map mkG refnLocals
        , \ xs -> let
            l1 = 1
            l2 = length refnTemplates
            l3 = length refnLocals
            refnPattern'   = fromGs $ take l1 xs
            refnTemplates' = fromGs $ take l2 $ drop l1 xs
            refnLocals'    = fromGs $ take l3 $ drop l2 $ drop l1 xs
            in if l1 == length refnPattern'  &&
                  l2 == length refnTemplates' &&
                  l3 == length refnLocals'
                  then RuleRefn
                        refnLevel
                        refnFilename
                        (head refnPattern')
                        refnTemplates'
                        refnLocals'
                  else gplateError "RuleRefn"
        )

instance MatchBind RuleRefn

instance ParsePrint RuleRefn where
    parse = do
        whiteSpace
        level     <- optionMaybe (brackets (fromInteger <$> integer))
        pattern   <- parse
        templates <- reservedOp "~~>" >> try (braces (parse `sepBy1` comma)) <|> (return <$> parse)
        locals    <- parse
        return $ RuleRefn level "" pattern templates locals
    pretty RuleRefn {..} = Pr.vcat $ concat [ [ Pr.brackets (Pr.int l), text "" ] | Just l <- [refnLevel] ]
                                  ++ [ pretty refnPattern <+> "~~>"
                                                          <+> case refnTemplates of
                                                                   [t] -> pretty t
                                                                   ts  -> prettyList Pr.braces Pr.comma ts ]
                                  ++ [ text "" ]
                                  ++ map pretty refnLocals



--------------------------------------------------------------------------------
-- Coerce instances ------------------------------------------------------------
--------------------------------------------------------------------------------

instance Coerce Expr Expr where
    promote = id
    demote = Just

instance Coerce Value Expr where
    promote (VHole x) = EHole x
    promote x = V x

    demote (EHole x) = Just $ VHole x
    demote (V x) = Just x
    demote _     = Nothing

instance Coerce Domain Expr where
    promote (DHole x) = EHole x
    promote x = D x

    demote (EHole x) = Just $ DHole x
    demote (D x) = Just x
    demote _     = Nothing


deepPromote :: GPlate a => a -> a
deepPromote = unliftG (bottomUp (liftG f))
    where
        f (V x) = promote x
        f (D x) = promote x
        f x     = promote x



--------------------------------------------------------------------------------
-- QuickCheck properties -------------------------------------------------------
--------------------------------------------------------------------------------

prop_CoerceExpr :: Expr -> Bool
prop_CoerceExpr = propCoerce

prop_CoerceValue :: Value -> Bool
prop_CoerceValue = propCoerce

prop_CoerceDomain :: Domain -> Bool
prop_CoerceDomain = propCoerce

propCoerce :: (Eq a, Coerce a Expr) => a -> Bool
propCoerce x = demote (promote x :: Expr) == Just x


prop_ParsePrintExpr :: Expr -> Bool
prop_ParsePrintExpr = propParsePrint

prop_ParsePrintValue :: Value -> Bool
prop_ParsePrintValue = propParsePrint

prop_ParsePrintDomain :: Domain -> Bool
prop_ParsePrintDomain = propParsePrint

prop_ParsePrintType :: Type -> Bool
prop_ParsePrintType = propParsePrint

propParsePrint :: (Eq a, ParsePrint a, GPlate a) => a -> Bool
propParsePrint a = Right a == parseEither (parse <* eof) (show $ pretty a)

