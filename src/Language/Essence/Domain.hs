{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Domain where

import Control.Applicative
import Control.Arrow ( first, second )
import Control.Monad ( ap, liftM, msum )
import Control.Monad.Error ( MonadError, ErrorT, runErrorT )
import Control.Monad.State ( MonadState )
import Control.Monad.Writer ( MonadWriter )
import Data.Generics ( Data )
import Data.List ( sort )
import Data.Maybe ( fromMaybe )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary, arbitrary, elements )
import Test.QuickCheck.Gen ( oneof )
import qualified Data.Text.Lazy as T

import Nested
import Constants
import Has
import GenericOps.Core ( NodeTag
                       , Hole, hole, HoleStatus(..)
                       , GPlate, gplate, gplateError, gplateLeaf, gplateSingle, gplateUniList
                       , GNode, mkG, fromG, fromGs
                       , MatchBind, match, BindingsMap, inScope
                       )
import ParsePrint
import PrintUtils ( (<+>), (<>), text, Doc )
import qualified PrintUtils as Pr

import Language.EssenceLexer
import Language.EssenceLexerP

import {-# SOURCE #-} Language.Essence.Expr
import                Language.Essence.Identifier
import                Language.Essence.Range
import                Language.Essence.Type
import {-# SOURCE #-} Language.Essence.Value ()
import {-# SOURCE #-} Language.EssenceEvaluator ( oldDeepSimplify, evaluate )



class DomainOf a where
    domainOf ::
        ( Applicative m
        , Has st BindingsMap
        , Has st [GNode]
        , MonadError (Nested Doc) m
        , MonadState st m
        , MonadWriter [Doc] m
        ) => a -> m Domain

needsRepresentation :: Domain -> Bool
needsRepresentation (DMatrix _ e) = needsRepresentation e
needsRepresentation (AnyDom {dConstr = TSet      }) = True
needsRepresentation (AnyDom {dConstr = TMSet     }) = True
needsRepresentation (AnyDom {dConstr = TFunction }) = True
needsRepresentation (AnyDom {dConstr = TRelation }) = True
needsRepresentation (AnyDom {dConstr = TPartition}) = True
needsRepresentation _ = False

representationValue :: Domain -> Maybe Expr
representationValue (AnyDom {dAttrs = DomainAttrs attrs}) = msum $ flip map attrs $ \ a -> case a of
    NameValue AttrRepresentation x -> Just x
    _ -> Nothing
representationValue _ = Nothing

domToType :: (Applicative m, MonadError (Nested Doc) m) => Domain -> m Type
domToType (DHole i) = return $ THole i
domToType DBool = return $ TBool
domToType (DInt _) = return $ TInt
domToType (DEnum i _) = return $ THole i
domToType (DUnnamed x) = return $ TUnnamed x
domToType (DMatrix i e) = TMatrix <$> domToType i <*> domToType e
domToType (AnyDom enum es _) = AnyType enum <$> mapM domToType es
domToType p@(Indices {}) = throwErrorSingle $ "not a valie type: " <+> pretty p


data Domain = DHole Identifier
    | DBool
    | DInt                (Range Expr)
    | DEnum    Identifier (Range Expr)
    | DUnnamed Expr
    | DMatrix  Domain Domain
    | AnyDom { dConstr  :: AnyTypeEnum
             , dElement :: [Domain]
             , dAttrs   :: DomainAttrs
             }
    | Indices Expr Expr
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag Domain

instance Hole Domain where
    hole (DHole (Identifier "_")) = UnnamedHole
    hole (DHole (Identifier nm) ) = NamedHole nm
    hole _                        = NotAHole

instance GPlate Domain where
    gplate   (DHole  x) = gplateSingle DHole x
    gplate p@(DBool {}) = gplateLeaf p
    gplate   (DInt   x) = gplateSingle DInt x
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
    gplate (Indices x y) = gplateUniList (\ [x',y'] -> Indices x' y' ) [x,y]

instance MatchBind Domain

instance ParsePrint Domain where
    parse = do

        lxm <- next

        case lxm of

            LIdentifier "indices" -> do
                parens $ do
                    i <- parse
                    comma
                    j <- parse
                    return (Indices i j)

            LIdentifier i -> let iden = Identifier (T.unpack i)
                             in  ( DEnum iden <$> parens parse )
                                 <||>
                                 ( return (DHole iden)         )

            L_bool -> return DBool

            L_int  -> DInt <$> (parens parse <||> return RAll)

            L_matrix -> do
                lexeme L_indexed
                lexeme L_by
                is <- brackets (parse `sepBy1` comma)
                lexeme L_of
                e  <- parse
                return $ foldr DMatrix e is

            L_tuple -> do
                es <- parens (parse `sepBy` comma)
                return $ AnyDom TTuple es (DomainAttrs [])

            L_OpenParen -> do
                es <- parse `sepBy` comma
                lexeme L_CloseParen
                return $ AnyDom TTuple es (DomainAttrs [])

            L_set -> do
                as <- parse
                lexeme L_of
                e  <- parse
                return $ AnyDom TSet [e] as

            L_mset -> do
                as <- parse
                lexeme L_of
                e  <- parse
                return $ AnyDom TMSet [e] as

            L_function -> do
                as <- parse
                fr <- parse
                lexeme L_LongArrow
                to <- parse
                return $ AnyDom TFunction [fr,to] as

            L_relation -> do
                as <- parse
                lexeme L_of
                es <- parens (parse `sepBy` lexeme L_Times)
                return $ AnyDom TRelation es as

            L_partition -> do
                as <- parse
                lexeme L_from
                e  <- parse
                return $ AnyDom TPartition [e] as

            _ -> failUnexpected lxm "domain"
        -- [ pBool, pInt, pEnum, pMatrix
        -- , pTuple, pSetMSet L_set TSet, pSetMSet L_mset TMSet
        -- , pFunction, pRelation, pPartition
        -- , pIndices
        -- , pDHole
        -- -- , between (lexeme L_BackTick) (lexeme L_BackTick) parse
        -- -- , between (reservedOp "'") (reservedOp "'") parse
        -- ]
        -- where
        --     pDHole = DHole <$> parse



    pretty (DHole (Identifier nm)) = text nm
    pretty DBool = "bool"
    pretty (DInt RAll) = "int"
    pretty (DInt r   ) = "int" <> Pr.parens (pretty r)
    pretty (DEnum i RAll) = pretty i
    pretty (DEnum i r   ) = pretty i <> Pr.parens (pretty r)
    pretty (DUnnamed i) = "unnamed" <+> pretty i
    pretty (DMatrix i e) = "matrix" <+> "indexed"
                       <+> "by" <+> prettyList Pr.brackets Pr.comma is
                       <+> "of" <+> pretty e'
        where
            (is,e') = helper i e
            helper a b = first (a:) $ case b of DMatrix c d -> helper c d
                                                _           -> ([], b)
    pretty (AnyDom TTuple es as) = "tuple" <+> pretty as <+> prettyList Pr.parens Pr.comma es
    pretty (AnyDom TSet  [e] as) = "set"  <+> pretty as <+> "of" <+> pretty e
    pretty (AnyDom TMSet [e] as) = "mset" <+> pretty as <+> "of" <+> pretty e
    pretty (AnyDom TFunction [fr,to] as) = "function"  <+> pretty as <+> pretty fr <+> "-->" <+> pretty to
    pretty (AnyDom TRelation es as) = "relation" <+> pretty as <+> "of"
                                                      <+> prettyList Pr.parens "*" es
    pretty (AnyDom TPartition [e] as) = "partition" <+> pretty as <+> "from" <+> pretty e
    pretty (Indices x y) = "indices" <> prettyList Pr.parens Pr.comma [x,y]
    pretty p = error ("Invalid domain: " ++ show p)

instance Arbitrary Domain where
    arbitrary = {-deepPromote <$> -}oneof
        [ DHole    <$> arbitrary
        , return DBool
        , DInt     <$> arbitrary
        , DEnum    <$> arbitrary <*> arbitrary
        , DUnnamed <$> arbitrary
        , DMatrix  <$> arbitrary <*> arbitrary
        , AnyDom TTuple     <$> arbitrary              <*> arbitrary
        , AnyDom TSet       <$> (return <$> arbitrary) <*> arbitrary
        , AnyDom TMSet      <$> (return <$> arbitrary) <*> arbitrary
        , do (fr,to) <- arbitrary; AnyDom TFunction [fr,to] <$> arbitrary
        , AnyDom TRelation  <$> arbitrary              <*> arbitrary
        , AnyDom TPartition <$> (return <$> arbitrary) <*> arbitrary
        ]

instance TypeOf Domain where
    typeOf p | trace TypeChecking ("typeOf Domain: " ++ show (pretty p)) False = undefined
    typeOf p@(DHole i)  = inScope (mkG p) $ typeOf i
    typeOf    DBool     = return TBool
    typeOf   (DInt  {}) = return TInt
    typeOf p@(DEnum      i _) = inScope (mkG p) $ typeOf i
    typeOf   (DUnnamed     x) = return $ TUnnamed x
    typeOf p@(DMatrix    a b) = inScope (mkG p) $ TMatrix `liftM` typeOf a `ap` typeOf b
    typeOf p@(AnyDom  e ds _) = inScope (mkG p) $ AnyType e `liftM` mapM typeOf ds
    typeOf p@(Indices m ind') = inScope (mkG p) $ do
        ind <- evaluate ind'
        let
            go :: Type -> Int -> Maybe Type
            go (TMatrix x _) 0 = return x
            go (TMatrix _ x) n = go x (n-1)
            go _ _ = Nothing

        tm <- typeOf =<< oldDeepSimplify m
        case go tm ind of
            Nothing -> throwErrorSingle $ "typeOf fail:" <+> pretty p
            Just t  -> return t

instance DomainOf Domain where
    domainOf = return



newtype DomainAttrs = DomainAttrs [DomainAttr]
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag DomainAttrs

instance Hole DomainAttrs

instance GPlate DomainAttrs where
    gplate (DomainAttrs xs) = gplateUniList DomainAttrs xs

instance MatchBind DomainAttrs where
    match p@(DomainAttrs ps) a@(DomainAttrs as) = inScope (mkG p, mkG a) $
        helper (DontCare `elem` ps)
               (sort $ filter (/=DontCare) ps)
               (sort $ filter (/=DontCare) as)
        where
            -- checkMatch :: DomainAttr -> DomainAttr -> m Bool
            checkMatch i j = do
                res <- runErrorT (match i j)
                case res of
                    Right _ -> return True
                    _       -> return False

            -- tryMatch :: DomainAttr -> [DomainAttr] -> m (Bool, [DomainAttr])
            tryMatch _ []     = return (False, [])
            tryMatch i (j:js) = do
                b <- checkMatch i j
                if b
                    then return (b,js)
                    else second (j:) `liftM` tryMatch i js

            -- helper :: Bool -> [DomainAttr] -> [DomainAttr] -> m ()
            helper _    []     []     = return ()  -- if both attr lists are fully consumed.
            helper True []     _      = return ()  -- if the pattern list is fully consumed, we DontCare.
            helper d    (x:xs) ys = do
                (res, ys') <- tryMatch x ys
                if res
                    then helper d xs ys'
                    else throwErrorSingle $ "attribute in pattern not found in actual: " <+> pretty x
            helper _ _ ys = throwErrorSingle $ "some attibutes in actual not matched: " <+> prettyList id Pr.comma ys

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
    parse = msum1 [pNameValue, pOnlyName, pDontCare]
        where
            pOnlyName  = OnlyName  <$> parse
            pNameValue = NameValue <$> parse <*> parse
            pDontCare  = DontCare  <$  lexeme (LIdentifier "_")
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
    | AttrMinOccur
    | AttrMaxOccur
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
    fromPairs =
            [ ( AttrRepresentation , L_representation )
            , ( AttrSize           , L_size           )
            , ( AttrMinSize        , L_minSize        )
            , ( AttrMaxSize        , L_maxSize        )
            , ( AttrMinOccur       , L_minOccur       )
            , ( AttrMaxOccur       , L_maxOccur       )
            , ( AttrTotal          , L_total          )
            , ( AttrPartial        , L_partial        )
            , ( AttrInjective      , L_injective      )
            , ( AttrSurjective     , L_surjective     )
            , ( AttrBijective      , L_bijective      )
            , ( AttrRegular        , L_regular        )
            , ( AttrComplete       , L_complete       )
            , ( AttrPartSize       , L_partSize       )
            , ( AttrMinPartSize    , L_minPartSize    )
            , ( AttrMaxPartSize    , L_maxPartSize    )
            , ( AttrNumParts       , L_numParts       )
            , ( AttrMinNumParts    , L_minNumParts    )
            , ( AttrMaxNumParts    , L_maxNumParts    )
            ]

instance Arbitrary DomainAttrEnum where
    arbitrary = elements [minBound .. maxBound]
