{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Expr where

import Control.Applicative
import Control.Arrow ( first )
import Control.Monad ( forM, liftM, unless, zipWithM_ )
import Control.Monad.Error ( MonadError, throwError )
import Control.Monad.State ( MonadState )
import Control.Monad.Writer ( MonadWriter )
import Data.Generics ( Data )
import Data.List ( minimumBy )
import qualified Data.Map as M
import qualified Data.Typeable as Typeable ( typeOf )
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Ord ( comparing )
import Data.String ( IsString, fromString )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary, arbitrary, shrink )
import Test.QuickCheck.Gen ( oneof )
import Unsafe.Coerce ( unsafeCoerce )


import GenericOps.Core
import Nested ( Nested )
import Has
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>), (<>), Doc )
import Utils
import qualified PrintUtils as Pr

import Language.EssenceLexer
import Language.EssenceLexerP

import                Language.Essence.Binding
import {-# SOURCE #-} Language.Essence.Domain
import                Language.Essence.Identifier
import {-# SOURCE #-} Language.Essence.Lambda
import                Language.Essence.Op
import {-# SOURCE #-} Language.Essence.OpDescriptor
import                Language.Essence.StructuredVar
import {-# SOURCE #-} Language.Essence.QuantifiedExpr
import                Language.Essence.Type
import {-# SOURCE #-} Language.Essence.Value
import                Language.Essence.Where
import {-# SOURCE #-} Language.EssenceEvaluator



data Expr = EHole Identifier
    | V Value
    | D Domain
    | Q QuantifiedExpr
    | L Lambda
    | Bubble Expr Expr [Either Binding Where]
    | EOp Op [Expr]
    | ETyped Expr Type
    | DimExpr Expr Domain
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

constr_Q :: QuantifiedExpr -> Expr
constr_Q = Q

prettyExprTopLevel :: Expr -> Doc
prettyExprTopLevel (Q i) = pretty i
prettyExprTopLevel i     = pretty i

isAtomicExpr :: Expr -> Bool
isAtomicExpr EHole   {} = True
isAtomicExpr V       {} = True
isAtomicExpr D       {} = True
isAtomicExpr L       {} = True
isAtomicExpr DimExpr {} = True
isAtomicExpr _          = False

instance IsString Expr where
    fromString = EHole . fromString

instance NodeTag Expr

instance Hole Expr where
    hole (EHole (Identifier "_")) = UnnamedHole
    hole (EHole (Identifier nm) ) = NamedHole nm
    hole _                        = NotAHole

instance GPlate Expr where
    gplate (EHole x) = gplateSingle EHole x
    gplate (V     x) = gplateSingle V     x
    gplate (D     x) = gplateSingle D     x
    gplate (Q     x) = gplateSingle Q     x
    gplate (L     x) = gplateSingle L     x
    -- gplate p@(Bubble {}) = gplateLeaf p
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
    gplate (ETyped x t) =
        ( [mkG x, mkG t]
        , \ ys -> let xs' = fromGs $ take 1 ys
                      ts' = fromGs $ take 1 $ drop 1 ys
                  in  case (xs', ts') of
                          ([x'],[t']) -> ETyped x' t'
                          _ -> gplateError "Expr.ETyped"
        )
    gplate (DimExpr x t) =
        ( [mkG x, mkG t]
        , \ ys -> let xs' = fromGs $ take 1 ys
                      ts' = fromGs $ take 1 $ drop 1 ys
                  in  case (xs', ts') of
                          ([x'],[t']) -> DimExpr x' t'
                          _ -> gplateError "Expr.DimExpr"
        )

instance MatchBind Expr where
    bind t = inScope (mkG t) $ do
        case hole t of                                                                -- check hole-status of the pattern.
            UnnamedHole  -> return Nothing -- gbindError "Unnamed hole in template."            -- unnamed hole in a template is just nonsense.
            NamedHole nm -> do
                oldNames <- getM
                if nm `elem` oldNames
                    then error $ "cyclic definition of something: " ++ nm ++ " " ++ show oldNames
                    else inScope nm $ do
                        bindings <- getM
                        res <- case M.lookup nm bindings of
                            Nothing -> return Nothing -- gbindError ("Not found: " <+> text nm)                                                     -- if the name cannot be found in te list of bindings.
                            Just (GNode ty_b b) | ty_b == Typeable.typeOf t
                                                    -> return (Just (unsafeCoerce b))                                                -- the name is bound to something of the expected type. great.
                                                | ty_b == Typeable.typeOf (undefined :: Domain)
                                                    -> return $ Just $ D $ unsafeCoerce b
                                                | ty_b == Typeable.typeOf (undefined :: StructuredVar)
                                                    -> case unsafeCoerce b of
                                                        I x -> return (Just $ EHole x)
                                                        _   -> return Nothing
                                                | otherwise        -> return Nothing
                                                -- | otherwise        -> gbindError $ vcat [ "Type mismatch for: " <+> text nm                         -- name is bound, but wrong type.
                                                --                                         , nest 4 "Expected: "   <+> text (show (typeOf t))
                                                --                                         , nest 4 "But got:  "   <+> text (show ty_b)
                                                --                                         ]
                        return res
            NotAHole -> case gplate t of
                            ([], _  ) -> return Nothing                       -- if this is not a hole, and is a leaf, just return it.
                            (ts, gen) -> do
                                -- ts'  <- mapM gbind ts                    -- otherwise, apply gbind recursively to immediate children.
                                (bools,ts') <- liftM unzip $ forM ts $ \ i -> do
                                            j <- gbind i
                                            case j of Nothing -> return (False, i)
                                                      Just k  -> return (True , k)
                                return $ if or bools
                                             then Just (gen ts')         -- degisen var.
                                             else Nothing


findPivotOp :: [Either Op Expr] -> Parser (Either Expr ([Either Op Expr], Op, [Either Op Expr]))
findPivotOp [Right x] = return $ Left x
findPivotOp xs = do
    let
        infixes = [ (i,f) | Left op <- xs
                          , OpInfix (i,f,_) _ <- [opDescriptor op]
                          ]

        (minP,minF) = minimumBy (comparing fst) infixes

        chck op = case opDescriptor op of
                    OpInfix (i,_,_) _ -> i == minP
                    _ -> False

        findFirst :: [Either Op Expr] -> Parser ([Either Op Expr], Op, [Either Op Expr])
        findFirst [] = failWithMsg "findPivotOp.findFirst"
        findFirst (Left i:is) | chck i = return ([], i, is)
        findFirst (i:is) = do
            (before, op, after) <- findFirst is
            return (i:before, op, after)

        findLast :: [Either Op Expr] -> Parser ([Either Op Expr], Op, [Either Op Expr])
        findLast is = do
            (before, op, after) <- findFirst (reverse is)
            return (reverse after, op, reverse before)

        findOnly :: [Either Op Expr] -> Parser ([Either Op Expr], Op, [Either Op Expr])
        findOnly is = do
            f <- findFirst is
            l <- findLast  is
            if f == l
                then return f
                else failWithMsg "Ambiguous use of non-associative operator."

    case (infixes,minF) of
        ( [], _      ) -> failWithMsg $ "Shunting Yard." <+> Pr.text (ppShow xs)
        ( _ , InfixL ) -> Right <$> findLast  xs
        ( _ , InfixN ) -> Right <$> findOnly  xs
        ( _ , InfixR ) -> Right <$> findFirst xs


shunt :: [Either Op Expr] -> Parser Expr
shunt xs = do
    result <- findPivotOp xs
    case result of
        Left x -> return x
        Right (before, op, after) -> do
            b <- shunt before
            a <- shunt after
            return $ EOp op [b,a]
-- shunt list = do
--     let result = S.evalState (worker list) ([],[])
--     result
--     where
-- 
--         worker :: [Either Op Expr] -> S.State ([Op],[Expr]) (Parser Expr)
-- 
--         worker [] = do
--             mcomp <- applyOp
--             case mcomp of
--                 Left  err   -> return err
--                 Right True  -> worker []
--                 Right False -> do
--                     st <- S.get
--                     case st of
--                         ([],[x]) -> return $ return x
--                         _        -> return $ failWithMsg $ "Failed" <+> Pr.text (show st)
-- 
--         worker (Left Negate : rest) = do
--             pushOp Negate
--             worker rest
-- 
--         worker (Left op : rest) = workerNegateOp op rest
-- 
--         worker (Right x : rest) = pushExpr x >> worker rest
-- 
--         workerNegateOp op rest = do
--             bOp <- checkCountOp (1 <=)
--             bX  <- checkCountExpr (1 <=)
--             if bOp && bX
--                 then do
--                     Just opOld <- popOp
--                     pushOp opOld
--                     if opOld == Negate
--                         then do
--                             applyOpResult <- applyOp
--                             case applyOpResult of
--                                 Left  p     -> return p
--                                 Right False -> return $ failWithMsg "error in Shunting Yard {1}"
--                                 Right True  -> do pushOp op
--                                                   worker rest
--                         else workerNormalOp op rest
--                 else workerNormalOp op rest
-- 
--         workerNormalOp op rest = do
--             bOp <- checkCountOp (1 <=)
--             bX  <- checkCountExpr (2 <=)
--             if bOp && bX
--                 then
--                     case opDescriptor op of
--                         OpInfix (opPrec, opFix, _) _ -> do
--                             Just opOld <- popOp
--                             case opDescriptor opOld of
--                                 OpInfix (opOldPrec, opOldFix, _) _ ->
--                                     case () of
--                                         _ | or [ opOld == Negate
--                                                , and [ opOldPrec > opPrec
--                                                      ]
--                                                ] -> do
--                                             pushOp opOld
--                                             applyOpResult <- applyOp
--                                             case applyOpResult of
--                                                 Left  p     -> return p
--                                                 Right False -> return $ failWithMsg "error in Shunting Yard {2}"
--                                                 Right True  -> do pushOp op
--                                                                   worker rest
--                                         _ | and [ opOldPrec == opPrec
--                                                 , opOldFix  == InfixN
--                                                 , opFix     == InfixN
--                                                 ] -> 
--                                             return $ failWithMsg $ "Ambiguous use of non-associative operator: \"" <> pretty op <> "\"."
--                                         _ -> do
--                                             pushOp opOld
--                                             pushOp op
--                                             worker rest
--                                 _            -> return $ failWithMsg $ "Operator is not infix:" <+> pretty opOld
--                         _            -> return $ failWithMsg $ "Operator is not infix:" <+> pretty op
--                 else do
--                     pushOp op
--                     worker rest
-- 
--         applyOp :: S.State ([Op],[Expr]) (Either (Parser Expr) Bool)
--         applyOp = do
--             st <- S.get
--             case st of
--                 (Negate:os,x1:ys) -> do
--                     S.put (os, EOp Negate [x1] : ys)
--                     return $ Right True
--                 (o:os,x1:x2:ys) ->
--                     case () of
--                         _ | o `elem` [HasDomain,HasType] ->
--                             case x1 of
--                                 D _ -> do
--                                     S.put (os, EOp o [x2,x1] : ys)
--                                     return $ Right True
--                                 _   ->
--                                     return $ Left $ failWithMsg "Has{Domain|Type} can only be used with a domain."
--                         _ -> do
--                             S.put (os, EOp o [x2,x1] : ys)
--                             return $ Right True
--                 _ -> return $ Right False
-- 
--         checkCountOp f = do
--             (ops,_) <- S.get
--             return $ f (length ops)
-- 
--         checkCountExpr f = do
--             (_,xs) <- S.get
--             return $ f (length xs)
-- 
--         pushOp   x = S.modify $ first  (x:)
--         pushExpr x = S.modify $ second (x:)
-- 
--         popOp = do
--             (ops,xs) <- S.get
--             case ops of
--                 (a:as) -> S.put (as,xs) >> return (Just a)
--                 _      -> return Nothing
-- 
--         -- popExpr = do
--         --     (ops,xs) <- S.get
--         --     case xs of
--         --         (a:as) -> S.put (ops,as) >> return (Just a)
--         --         _      -> return Nothing
-- 
--         -- evalStacks [op] [a]   = EOp op [a]
--         -- evalStacks [op] [a,b] = EOp op [a,b]

minusToNegate :: [Either Op Expr] -> Parser [Either Op Expr]
minusToNegate ls = worker $ let (a,b) = span (==Left Minus) ls
                            in  map (\ _ -> Left Negate) a ++ b
    where
        worker (Left op1 : Left Minus : rest) = do
            let (a,bs) = span (==Left Minus) rest
            let a' = Left Negate : map (\ _ -> Left Negate) a
            bs' <- worker bs
            return $ Left op1 : a' ++ bs'
        worker (Left op1 : Left op2 : _)
            | op2 /= Negate
            = failWithMsg $ "Two operator symbols next to each other," <+> Pr.text (show op1) <+> "and" <+> Pr.text (show op2) <> "."
        worker (a:rest) = do as <- worker rest; return (a:as)
        worker []       = return []

applyNegate :: [Either Op Expr] -> [Either Op Expr]
applyNegate [] = []
applyNegate (Left Negate : Right x     : xs) = Right (EOp Negate [x]) : applyNegate xs
applyNegate (Left Negate : Left Negate : xs) = applyNegate xs
applyNegate (x:xs) = x : applyNegate xs

pDimExpr :: Parser Expr
pDimExpr = flip (<?>) "DimExpr" $ do
    lexeme L_find
    x <- pCorePrePost
    colon
    d <- parse
    return $ DimExpr x d

pCorePrePost :: Parser Expr
pCorePrePost = applyPrefix prefixes $ applyPostFix postfixes pCore
    where
        prefixes :: Parser (Expr -> Expr)
        prefixes = do
            let allP = flip mapMaybe allValues $ \ op -> case opDescriptor op of
                        OpPrefix pa _ -> Just pa
                        _ -> Nothing
            fs <- some (msum1 allP)
            return $ foldr1 (.) fs

        postfixes :: Parser (Expr -> Expr)
        postfixes = do
            let allP = flip mapMaybe allValues $ \ op -> case opDescriptor op of
                        OpPostfix pa _ -> Just pa
                        _ -> Nothing
            let pImage = do
                    ys <- parens (parse `sepBy1` comma)
                    return $ \ x -> case ys of
                        [y] -> EOp Image [x,y]
                        _   -> EOp Image [x,toVTuple ys]
            fs <- some (msum1 $ pImage : allP)
            return $ foldr1 (.) (reverse fs)

        applyPrefix :: Parser (Expr -> Expr) -> Parser Expr -> Parser Expr
        applyPrefix pre item = item <||>
            ( do f <- pre; x <- item; return (f x) )

        applyPostFix :: Parser (Expr -> Expr) -> Parser Expr -> Parser Expr
        applyPostFix post item = do
            x  <- item
            mf <- optionMaybe post
            case mf of
                Nothing -> return x
                Just f  -> return (f x)

pCore :: Parser Expr
pCore = msum1 $
        [ pBubble
        , lexeme L_lambda >>                                  L     <$> parse  <?>  "lambda expression"
        ,                                                     Q     <$> parse  <?>  "quantified expression"
        ,                                                     EHole <$> parse  <??> "identifier"
        , parens parse                                                         <??> "(expression)"
        ,                                                     V     <$> parse  <??> "constant"
        , ( between (lexeme L_BackTick) (lexeme L_BackTick) $ D     <$> parse) <??> "`domain`"
        ] ++ pLispyAndSpecial
    where
        pLispyAndSpecial :: [Parser Expr]
        pLispyAndSpecial = flip mapMaybe allValues $ \ op -> case opDescriptor op of
            OpLispy   pa _ -> Just pa
            OpSpecial pa _ -> Just pa
            _ -> Nothing

        pBubble :: Parser Expr
        pBubble = flip (<?>) "bubble" $ parens $ do
            x  <- parse
            lexeme L_At
            bs <- optionMaybe $ braces parse
            y  <- parse
            return $ Bubble x y (fromMaybe [] bs)

instance ParsePrint Expr where
    parse = flip (<?>) "malformed expression" $ do
        xs   <- parseExprOpList pCorePrePost
        xs'  <- minusToNegate xs
        shunt $ applyNegate xs'
        where
            parseExprOpList :: Parser Expr -> Parser [Either Op Expr]
            -- parseExprOpList p = some $ ( Right <$> p <||> Left <$> msum1 infixOps )
            parseExprOpList p = some $ Left  <$> (msum1 infixOps <??> "infix operator")
                                  <||> Right <$> p

            infixOps :: [Parser Op]
            infixOps = flip mapMaybe allValues $ \ op -> case opDescriptor op of
                        OpInfix _ _ -> case opFace op of
                                            Nothing -> Nothing
                                            Just l  -> Just $ do lexeme l; return op
                        _           -> Nothing

    pretty (EHole x) = pretty x
    pretty (V     x) = pretty x
    pretty (D     x) = pretty x
    pretty (Q     x) = Pr.parens (pretty x)
    pretty (L     x) = "lambda" <+> pretty x
    pretty (Bubble x y bs) = Pr.parens (pretty x <+> "@" <+> bsPretty <+> pretty y)
        where
            bsPretty = case bs of [] -> Pr.empty
                                  _  -> Pr.braces $ Pr.vcat (map pretty bs)
    pretty p@(EOp {}) = prettyOp 0 p
        where
            prettyOp :: Int -> Expr -> Pr.Doc
            -- prettyOp i x | trace msg False = undefined
            --     where msg = " -- prettyOp " ++ show i ++ " " ++ show x
            prettyOp _ param@(EOp Index   _) = let OpPostfix _ pr = opDescriptor Index   in pr param
            prettyOp _       (EOp Image [f, V i@(VTuple _)]) = pretty f <> pretty i
            prettyOp _       (EOp Image [f,   i           ]) = pretty f <> Pr.parens (pretty i)
            prettyOp _ param@(EOp Replace _) = let OpPostfix _ pr = opDescriptor Replace in pr param
            prettyOp envPrec param@(EOp op xs) = case (opDescriptor op, xs) of
                (OpLispy   _ pr, _    ) -> pr xs
                (OpInfix   _ pr, [i,j]) -> pr prettyOp envPrec i j
                (OpPrefix  _ pr, [i]  ) -> pr i
                (OpPostfix _ pr, [i]  ) -> pr i
                (OpSpecial _ pr, _    ) -> pr param
                _ -> error $ "prettyOp: " ++ show param
            prettyOp _ (D x) = "`" <> pretty x <> "`"
            prettyOp _ x = pretty x
    pretty (ETyped  x t) = Pr.parens $ pretty x <+> Pr.colon <+> "`" <> pretty t <> "`"
    pretty (DimExpr x d) = "find" <+> pretty x <+> Pr.colon <+> pretty d

instance Arbitrary Expr where
    arbitrary = {-deepPromote <$> -}oneof
        [ EHole <$> arbitrary
        , V     <$> arbitrary
        , D     <$> arbitrary
        ]
    shrink (V x) = map V $ shrink x
    shrink (D x) = map D $ shrink x
    shrink (EOp op xs) = map (EOp op) $ mapM shrink xs
    shrink _     = []

instance TypeOf Expr where
    typeOf p@(EHole  i    ) = inScope (mkG p) $ typeOf i
    typeOf p@(V      v    ) = inScope (mkG p) $ typeOf v
    typeOf p@(D      d    ) = inScope (mkG p) $ typeOf d
    typeOf p@(Q      q    ) = inScope (mkG p) $ typeOf q
    typeOf p@(Bubble x _ _) = inScope (mkG p) $ typeOf x

    typeOf DimExpr {} = return TBool

    typeOf p@(ETyped x t) = inScope (mkG p) $ do
        tx <- typeOf x
        if typeHasUnknowns tx
            then return t
            else if t == tx
                    then return t
                    else throwError "Supplied type doesn't match the inferred type."

    typeOf p@(EOp op [x,y])
        | op `elem` [Plus,Minus,Times,Div,Mod,Pow]
        = typeOfOp p [x,y] [(TInt==),(TInt==)] TInt

    typeOf p@(EOp op [x])
        | op `elem` [Negate,Factorial]
        = typeOfOp p [x] [(TInt==)] TInt

    typeOf p@(EOp op [x,y])
        | op `elem` [Lt,Leq,Gt,Geq]
        = inScope (mkG p) $ do
            tx <- typeOf x
            ty <- typeOf y
            bx <- isOrderedType tx
            by <- isOrderedType ty
            if tx /= ty
                then typeErrorBinOp tx ty "Type error: Comparing incompatible types."
                else if bx && by
                         then return TBool
                         else typeErrorBinOp tx ty "Type error: Comparing unordered types."

    typeOf p@(EOp op [x,y])
        | op `elem` [Neq,Eq]
        = inScope (mkG p) $ do
            tx <- typeOf x
            ty <- typeOf y
            if tx == ty
                then return TBool
                else typeErrorBinOp tx ty "Type error: Equality on incompatible types."

    typeOf p@(EOp Not [x])
        = typeOfOp p [x] [(TBool==)] TBool

    typeOf p@(EOp op [x,y])
        | op `elem` [Or,And,Imply,Iff]
        = typeOfOp p [x,y] [(TBool==),(TBool==)] TBool

    typeOf p@(EOp op [x,y])
        | op `elem` [Union,Intersect]
        = inScope (mkG p) $ do
            tx <- typeOf x
            ty <- typeOf y
            case (tx,ty) of
                (AnyType t1 a, AnyType t2 b)
                    | t1 == t2
                    , t1 == TSet || t1 == TMSet
                    , a == b -> return $ AnyType t1 a
                _ -> typeErrorBinOp tx ty "Type error."

    typeOf p@(EOp op [x,y])
        | op `elem` [Subset,SubsetEq,Supset,SupsetEq]
        = inScope (mkG p) $ do
            tx <- typeOf x
            ty <- typeOf y
            case (tx,ty) of
                (AnyType t1 a, AnyType t2 b)
                    | t1 == t2
                    , t1 == TSet || t1 == TMSet
                    , a == b -> return TBool
                _ -> typeErrorBinOp tx ty "Type error."

    typeOf p@(EOp In [x,y])
        = inScope (mkG p) $ do
            tx <- typeOf x
            ty <- typeOf y
            case ty of
                AnyType te [ty']
                    | te `elem` [TSet,TMSet]
                    , tx `typeUnify` ty' -> return TBool
                _ -> typeErrorBinOp tx ty "Type error."

    typeOf p@(EOp op [x,y])
        | op `elem` [Max,Min]
        = inScope (mkG p) $ do
            tx <- typeOf x
            ty <- typeOf y
            bx <- isOrderedType tx
            by <- isOrderedType ty
            if tx /= ty
                then typeErrorBinOp tx ty "Type error: Comparing incompatible types."
                else if bx && by
                         then return tx
                         else typeErrorBinOp tx ty "Type error: Comparing unordered types.";

    typeOf p@(EOp op [x])
        | op `elem` [Max,Min]
        = inScope (mkG p) $ do
            tx <- typeOf x
            case tx of
                AnyType te [tx'] | te `elem` [TSet,TMSet] -> do
                    tb' <- isOrderedType tx'
                    if tb'
                        then return tx'
                        else typeErrorUnOp tx "Type error."
                _ -> typeErrorUnOp tx "Type error."

    typeOf p@(EOp TwoBars [x]) = inScope (mkG p) $ do
        tx <- typeOf x
        case tx of
            TInt -> return TInt
            AnyType te _
                 | te `elem` [TSet, TMSet, TFunction, TRelation, TPartition]
                 -> return TInt
            _    -> typeErrorUnOp tx "Type error."

    typeOf p@(EOp ToSet [x]) = inScope (mkG p) $ do -- TODO toSet(dom)
        tx <- typeOf x
        case tx of
            AnyType TMSet     [te] -> return $ AnyType TSet [te]
            AnyType TRelation tes  -> return $ AnyType TSet [AnyType TTuple tes]
            AnyType TFunction tes  -> return $ AnyType TSet [AnyType TTuple tes]
            TMatrix _ te           -> return $ AnyType TSet [te]
            _ -> typeErrorUnOp tx "Type error."

    typeOf p@(EOp ToMSet [x]) = inScope (mkG p) $ do -- TODO toSet(dom)
        tx <- typeOf x
        case tx of
            AnyType TSet      [te] -> return $ AnyType TMSet [te]
            AnyType TRelation tes  -> return $ AnyType TMSet [AnyType TTuple tes]
            AnyType TFunction tes  -> return $ AnyType TMSet [AnyType TTuple tes]
            TMatrix _ te           -> return $ AnyType TSet [te]
            _ -> typeErrorUnOp tx "Type error."

    typeOf p@(EOp ToRelation [x]) = inScope (mkG p) $ do
        tx <- typeOf x
        case tx of
            AnyType TFunction tes -> return $ AnyType TRelation tes
            _ -> typeErrorUnOp tx "Type error."

    typeOf p@(EOp Defined [x]) = inScope (mkG p) $ do
        tx <- typeOf x
        case tx of
            AnyType TFunction [a,_] -> return $ AnyType TSet [a]
            _ -> typeErrorUnOp tx "Type error."

    typeOf p@(EOp Range [x]) = inScope (mkG p) $ do
        tx <- typeOf x
        case tx of
            AnyType TFunction [_,b] -> return $ AnyType TSet [b]
            _ -> typeErrorUnOp tx "Type error."

    typeOf p@(EOp Image [x,y]) = inScope (mkG p) $ do
        tx <- typeOf x
        ty <- typeOf y
        case tx of
            AnyType TFunction [a,b] | a == ty -> return b
            AnyType TRelation tes -> case ty of
                AnyType TTuple tes' | tes == tes' -> return TBool
                AnyType TTuple tes' |  length tes == length tes'
                                    && any (==TUnknown) tes' -> do
                    rs <- sequence
                            [ if b == TUnknown
                                then return a
                                else if a == b
                                        then return a
                                        else typeError "Type error."
                            | (a,b) <- zip tes tes'
                            ]
                    return $ AnyType TRelation rs
                _ -> typeError "Type error."
            _ -> typeError "Type error."

    typeOf p@(EOp PreImage [x,y]) = inScope (mkG p) $ do
        tx <- typeOf x
        ty <- typeOf y
        case tx of
            AnyType TFunction [a,b] | b == ty -> return $ AnyType TSet [a]
            _ -> typeError "Type error."

    typeOf p@(EOp Inverse [x,y]) = inScope (mkG p) $ do
        tx <- typeOf x
        ty <- typeOf y
        case (tx,ty) of
            (AnyType TFunction [a,b], AnyType TFunction [c,d])
                | a == d
                , b == c -> return TBool
            _ -> typeError "Type error."

    typeOf p@(EOp op [x,y,z])
        | op `elem` [Together,Apart]
        = inScope (mkG p) $ do
        tx <- typeOf x
        ty <- typeOf y
        tz <- typeOf z
        case tz of
            AnyType TPartition [te]
                | te == tx
                , te == ty -> return TBool
            _ -> typeError "Type error."

    typeOf p@(EOp Party [x,y]) = inScope (mkG p) $ do
        tx <- typeOf x
        ty <- typeOf y
        case ty of
            AnyType TPartition [te]
                | te == tx -> return $ AnyType TSet [te]
            _ -> typeError "Type error."

    typeOf p@(EOp Participants [x]) = inScope (mkG p) $ do
        tx <- typeOf x
        case tx of
            AnyType TPartition [te] -> return $ AnyType TSet [te]
            _ -> typeError "Type error."

    typeOf p@(EOp Parts [x]) = inScope (mkG p) $ do
        tx <- typeOf x
        case tx of
            AnyType TPartition [te] -> return $ AnyType TSet [AnyType TSet [te]]
            _ -> typeError "Type error."

    typeOf p@(EOp Freq [x,y]) = inScope (mkG p) $ do
        tx <- typeOf x
        ty <- typeOf y
        case tx of
            AnyType TMSet [te] | te == ty -> return TInt
            TMatrix _      te  | te == ty -> return TInt
            _ -> typeError "Type error."

    typeOf p@(EOp Hist [x,y]) = inScope (mkG p) $ do
        tx <- typeOf x
        ty <- typeOf y
        case (tx,ty) of
            (AnyType TMSet [te], TMatrix ti te')
                | te == te' -> return $ TMatrix ti TInt
            (TMatrix _      te,  TMatrix ti te')
                | te == te' -> return $ TMatrix ti TInt
            _ -> typeError "Type error."

    typeOf p@(EOp Index [_,D _]) = inScope (mkG p) $ typeError "Type error in matrix slicing, not implemented."

    typeOf p@(EOp Index [x,y]) = inScope (mkG p) $ do
        tx <- typeOf x
        ty <- typeOf y
        case (tx, ty) of
            (TMatrix ti te, tj) | ti == tj -> do
                bi <- isOrderedType ti
                if bi
                    then return te
                    else typeErrorBinOp tx ty "Type error in indexing. Must use an ordered type."
            (AnyType TTuple tis, TInt) -> do
                val <- evaluate y
                if val >= 1 && val <= length tis
                    then return (tis !! (val - 1))
                    else typeErrorBinOp tx ty $ "Type error in indexing. Value out of bounds: " <+> Pr.text (show val)
            _ -> typeErrorBinOp tx ty "Type error in indexing."

    typeOf   (EOp HasType   _) = return TBool
    typeOf   (EOp HasDomain _) = return TBool
    typeOf p@(EOp Replace   [a,_,_]) = inScope (mkG p) $ typeOf a

    typeOf p@(EOp AllDiff [x]) = inScope (mkG p) $ do
        tx <- typeOf x
        case tx of
            TMatrix {} -> return TBool
            _          -> typeErrorUnOp tx "Type error."

    typeOf p@(EOp ToInt [x])
        = typeOfOp p [x] [(TBool==)] TInt

    typeOf p@(EOp Flatten [x]) = inScope (mkG p) $ do
        tx <- typeOf x
        let
            collectIndices :: Type -> ([Type],Type)
            collectIndices (TMatrix i e) = first (i:) $ collectIndices e
            collectIndices e = ([], e)
        let (_,e) = collectIndices tx
        case tx of
            TMatrix {} -> return $ TMatrix TInt e
            _          -> typeError "flatten works on matrices only."

    typeOf p@(EOp Flatten [x,y]) = inScope (mkG p) $ do
        val <- evaluate y
        tx <- typeOf x
        let
            collectIndices :: Type -> Int -> ([Type],Type)
            collectIndices e 0 = ([],e)
            collectIndices (TMatrix i e) n = first (i:) $ collectIndices e (n-1)
            collectIndices e _ = ([], e)
        let (_,e) = collectIndices tx val
        case tx of
            TMatrix {} -> return $ TMatrix TInt e
            _          -> typeError "flatten works on matrices only."

    typeOf p@(EOp NormIndices [x]) = inScope (mkG p) $ do
        tx <- typeOf x
        case tx of
            TMatrix i e -> do
                b <- isOrderedType i
                if b
                    then return $ TMatrix i e
                    else typeError "normIndices works on matrices indexed by an ordered type only."
            _ -> typeError "normIndices works on matrices only."

    typeOf p = inScope (mkG p) $ typeError "Type error in expression."

    -- typeOf
    -- | V Value
    -- | D Domain
    -- | Q QuantifiedExpr
    -- | Bubble Expr Expr [Either Binding Where]
    -- | EOp Op [Expr]

-- typeOfOp :: (TypeOf a, Monad m) => Expr -> [a] -> [Type -> Bool] -> Type -> m Type
typeOfOp ::
    ( Applicative m
    , Has st BindingsMap
    , Has st [GNode]
    , Has st [(GNode,GNode)]
    , MonadWriter [Doc] m
    , MonadState st m
    , MonadError (Nested Doc) m
    , GPlate a
    , GPlate b
    , TypeOf b
    ) => a -> [b] -> [Type -> Bool] -> t -> m t
typeOfOp p xs fs tres = inScope (mkG p) $ do
    txs <- mapM typeOf xs
    case txs of
        [tx]    -> zipWithM_ (\ f t -> unless (f t) (typeErrorUnOp  tx    "Type error in unary operator.")) fs txs
        [tx,ty] -> zipWithM_ (\ f t -> unless (f t) (typeErrorBinOp tx ty "Type error in binary operator.")) fs txs
        _       -> zipWithM_ (\ f t -> unless (f t) (typeError "Type error in operator.")) fs txs
    return tres


instance DomainOf Expr where
    domainOf (EHole i) = domainOf i
    domainOf (D     d) = return d
    domainOf _ = throwError "domainOf Expr not implemented."

