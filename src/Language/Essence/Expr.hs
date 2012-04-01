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
    typeOf p@(EOp Plus [x,y]) = typeOfOp p [x,y] [(TInt==),(TInt==)] TInt
    typeOf p@(EOp Max  [x]  ) = typeOfOp p [x]   [(AnyType TSet [TInt]==)] TInt
    typeOf p@(EOp Max  [x,y]) = typeOfOp p [x,y] [(TInt==),(TInt==)] TInt
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
    , Has st [GNode]
    , Has st BindingsMap
    , MonadWriter [Doc] m
    , MonadState st m
    , MonadError Doc m
    , GPlate a
    , GPlate b
    , TypeOf b
    ) => a -> [b] -> [Type -> Bool] -> t -> m t
typeOfOp p xs fs ty = inScope (mkG p) $ do
    txs <- mapM typeOf xs
    zipWithM_ (\ f t -> unless (f t) (typeError "Type error in operator.")) fs txs
    return ty



instance DomainOf Expr where
    domainOf (EHole i) = domainOf i
    domainOf (D     d) = return d
    domainOf _ = throwError "domainOf Expr not implemented."


intLike :: Type -> Bool
intLike TBool = True
intLike TInt = True
intLike _ = False


typeError ::
    ( Applicative m
    , Has st [GNode]
    , MonadError Doc m
    , MonadState st m
    ) => Doc -> m a
typeError s = do
    nodes :: [GNode]
          <- take 3 <$> getM
    throwError $ Pr.vcat $ s : [ Pr.nest 4 $ "in: " <+> pretty n
                               | GNode _ n <- nodes
                               ]



