{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Expr where

import Control.Applicative
import Control.Monad ( forM, liftM, unless, zipWithM_ )
import Control.Monad.Error ( MonadError, throwError )
import Control.Monad.Identity ( Identity )
import Control.Monad.State ( MonadState )
import Control.Monad.Writer ( MonadWriter )
import Data.Function ( on )
import Data.Generics ( Data )
import Data.List ( groupBy, sortBy )
import Data.Maybe ( fromMaybe, mapMaybe )
import Data.Ord ( comparing )
import Data.String ( IsString, fromString )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import qualified Data.Map as M
import qualified Data.Typeable as Typeable ( typeOf )
import Test.QuickCheck ( Arbitrary, arbitrary, shrink )
import Test.QuickCheck.Gen ( oneof )
import Unsafe.Coerce ( unsafeCoerce )

import Has
import GenericOps.Core
import ParsecUtils
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>), Doc )
import qualified PrintUtils as Pr
import Utils

import {-# SOURCE #-} Language.Essence.Binding
import {-# SOURCE #-} Language.Essence.Domain
import                Language.Essence.Identifier
import                Language.Essence.Op
import {-# SOURCE #-} Language.Essence.OpDescriptor
import {-# SOURCE #-} Language.Essence.QuantifiedExpr
import                Language.Essence.Type
import {-# SOURCE #-} Language.Essence.Value
import                Language.Essence.Where
import {-# SOURCE #-} Language.EssenceEvaluator



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
    gplate (EHole x) = gplateSingle EHole x
    gplate (V     x) = gplateSingle V     x
    gplate (D     x) = gplateSingle D     x
    gplate (Q     x) = gplateSingle Q     x
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
                            Just (GNode ty_b b) | Typeable.typeOf t == ty_b -> return (Just (unsafeCoerce b))                                                -- the name is bound to something of the expected type. great.
                            Just (GNode ty_b b) | Typeable.typeOf (undefined :: Domain) == ty_b -> return $ Just $ D $ unsafeCoerce b
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
                            _   -> EOp Image [x,toVTuple ys]
                fs <- many1 (choiceTry $ pImage : allP)
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
                    . reverse
                    . sortBy  (comparing fst)
                    ) infixes

            core :: Parser Expr
            core = choiceTry $ [ pBubble
                               , Q     <$> parse
                               , EHole <$> parse
                               , parens    parse
                               , V     <$> parse
                               , D     <$> parse
                               ] ++ pLispyAndSpecial

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
    pretty p@(EOp {}) = prettyOp 0 p
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
                else typeErrorBinOp tx ty $ "Type error: Equality on incompatible types."

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
                    , tx == ty' -> return TBool
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
                         then return TBool
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
            _ -> typeErrorUnOp tx "Type error."

    typeOf p@(EOp ToMSet [x]) = inScope (mkG p) $ do -- TODO toSet(dom)
        tx <- typeOf x
        case tx of
            AnyType TSet      [te] -> return $ AnyType TMSet [te]
            AnyType TRelation tes  -> return $ AnyType TMSet [AnyType TTuple tes]
            AnyType TFunction tes  -> return $ AnyType TMSet [AnyType TTuple tes]
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
    , MonadError Doc m
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

