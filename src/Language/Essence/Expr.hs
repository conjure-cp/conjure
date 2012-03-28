{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}

module Language.Essence.Expr where

import Control.Applicative
import Control.Arrow ( first, second )
import Control.Monad.Error ( throwError )
import Control.Monad.Identity ( Identity )
import Control.Monad.State
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

import GenericOps.Core
import ParsecUtils
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>) )
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
    bind t = do
        lift $ lift $ modify $ first (mkG t :)                                  -- add this node on top of the call stack.
        result <- case hole t of                                                -- check hole-status of the pattern.
            UnnamedHole  -> return Nothing -- gbindError "Unnamed hole in template."  -- unnamed hole in a template is just nonsense.
            NamedHole nm -> do
                oldNames <- lift $ lift $ gets snd
                if nm `elem` oldNames
                    then error $ "cyclic definition of something: " ++ nm ++ " " ++ show oldNames
                    else do
                        lift $ lift $ modify $ second (nm :)
                        bindings <- lift get
                        res <- case M.lookup nm bindings of
                            Nothing -> return Nothing -- gbindError ("Not found: " <+> text nm)                                                     -- if the name cannot be found in te list of bindings.
                            Just (GNode ty_b b) | Typeable.typeOf t == ty_b -> return (Just (unsafeCoerce b))                                                -- the name is bound to something of the expected type. great.
                            Just (GNode ty_b b) | Typeable.typeOf (undefined :: Domain) == ty_b -> return $ Just $ D $ unsafeCoerce b
                                                | otherwise        -> return Nothing
                                                -- | otherwise        -> gbindError $ vcat [ "Type mismatch for: " <+> text nm                         -- name is bound, but wrong type.
                                                --                                         , nest 4 "Expected: "   <+> text (show (typeOf t))
                                                --                                         , nest 4 "But got:  "   <+> text (show ty_b)
                                                --                                         ]
                        lift $ lift $ modify $ second tail
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
        lift $ lift $ modify (first tail)
        return result

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
    typeOf (EHole i) = typeOf i
    typeOf (V v) = typeOf v
    typeOf (D d) = typeOf d
    typeOf (Q q) = typeOf q
    typeOf (Bubble x _ _) = typeOf x
    typeOf p@(EOp Plus [x,y]) = do
        tx <- typeOf x
        unless (intLike tx) $ throwError $ Pr.vcat [ "Type mismatch."
                                                   , Pr.nest 4 $ "in:" <+> pretty x
                                                   , Pr.nest 4 $ "in:" <+> pretty p
                                                   ]
        ty <- typeOf y
        unless (intLike ty) $ throwError $ Pr.vcat [ "Type mismatch."
                                                   , Pr.nest 4 $ "in:" <+> pretty y
                                                   , Pr.nest 4 $ "in:" <+> pretty p
                                                   ]
        return TInt
    typeOf _ = throwError "typeOf Expr not implemented."
    -- typeOf
    -- | V Value
    -- | D Domain
    -- | Q QuantifiedExpr
    -- | Bubble Expr Expr [Either Binding Where]
    -- | EOp Op [Expr]

instance DomainOf Expr where
    domainOf (EHole i) = domainOf i
    domainOf (D     d) = return d
    domainOf _ = throwError "domainOf Expr not implemented."


intLike :: Type -> Bool
intLike TBool = True
intLike TInt = True
intLike _ = False
