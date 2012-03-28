{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.QuantifiedExpr where

import Control.Applicative
import Control.Monad.Error ( throwError )
import Control.Monad.State ( get )
import Data.Generics ( Data )
import Data.Maybe ( mapMaybe, maybeToList )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import qualified  Data.Map as M

import GenericOps.Core ( NodeTag, Hole
                       , GPlate, gplate, gplateError, gplateUniList
                       , mkG, fromGs
                       , MatchBind, match, bind, getBinding )
import ParsecUtils
import ParsePrint
import PrintUtils ( (<>), (<+>), text )
import qualified PrintUtils as Pr

import Language.Essence.Domain
import Language.Essence.Expr
import Language.Essence.Identifier
import Language.Essence.Op
import Language.Essence.QuantifierDecl
import Language.Essence.Type



data QuantifiedExpr = QuantifiedExpr
    { quanName     :: Identifier
    , quanVar      :: Identifier
    , quanOverDom  :: Maybe Domain
    , quanOverExpr :: Maybe (Op, Expr)
    , quanGuard    :: QuanGuard
    , quanBody     :: Expr
    }
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag QuantifiedExpr

instance Hole QuantifiedExpr

instance GPlate QuantifiedExpr where
    gplate (QuantifiedExpr qnName qnVar Nothing Nothing qnGuard qnBody) =
        ( [ mkG (EHole qnName), mkG (EHole qnVar), mkG qnBody, mkG qnGuard ]
        , \ xs -> let qnName'   = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 xs
                      qnVar'    = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 1 xs
                      qnBody'   = fromGs $ take 1 $ drop 2 xs
                      qnGuard'  = fromGs $ take 1 $ drop 3 xs
                  in  if length qnName'   == 1 &&
                         length qnVar'    == 1 &&
                         length qnBody'   == 1 &&
                         length qnGuard'  == 1
                         then QuantifiedExpr (head qnName')
                                             (head qnVar')
                                             Nothing
                                             Nothing
                                             (head qnGuard')
                                             (head qnBody')
                         else gplateError "QuantifiedExpr[1]"
        )
    gplate (QuantifiedExpr qnName qnVar Nothing (Just (qnOp, qnExpr)) qnGuard qnBody) =
        ( [ mkG (EHole qnName), mkG (EHole qnVar), mkG qnBody, mkG qnGuard, mkG qnOp, mkG qnExpr ]
        , \ xs -> let qnName'   = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 xs
                      qnVar'    = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 1 xs
                      qnBody'   = fromGs $ take 1 $ drop 2 xs
                      qnGuard'  = fromGs $ take 1 $ drop 3 xs
                      qnOp'     = fromGs $ take 1 $ drop 4 xs
                      qnExpr'   = fromGs $ take 1 $ drop 5 xs
                  in  if length qnName'   == 1 &&
                         length qnVar'    == 1 &&
                         length qnBody'   == 1 &&
                         length qnOp'     == 1 &&
                         length qnExpr'   == 1 &&
                         length qnGuard'  == 1
                         then QuantifiedExpr (head qnName')
                                             (head qnVar')
                                             Nothing
                                             (Just (head qnOp', head qnExpr'))
                                             (head qnGuard')
                                             (head qnBody')
                         else gplateError "QuantifiedExpr[2]"
        )
    gplate (QuantifiedExpr qnName qnVar (Just qnDom) Nothing qnGuard qnBody) =
        ( [ mkG (EHole qnName), mkG (EHole qnVar), mkG qnBody, mkG qnGuard, mkG qnDom ]
        , \ xs -> let qnName'   = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 xs
                      qnVar'    = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 1 xs
                      qnBody'   = fromGs $ take 1 $ drop 2 xs
                      qnGuard'  = fromGs $ take 1 $ drop 3 xs
                      -- qnDom'    = mapMaybe (\ j -> case j of D     i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 3 xs
                      qnDom'    = fromGs $ take 1 $ drop 4 xs
                  in  if length qnName'   == 1 &&
                         length qnVar'    == 1 &&
                         length qnBody'   == 1 &&
                         length qnDom'    == 1 &&
                         length qnGuard'  == 1
                         then QuantifiedExpr (head qnName')
                                             (head qnVar')
                                             (Just (head qnDom'))
                                             Nothing
                                             (head qnGuard')
                                             (head qnBody')
                         else gplateError "QuantifiedExpr[3]"
        )
    gplate (QuantifiedExpr qnName qnVar (Just qnDom) (Just (qnOp, qnExpr)) qnGuard qnBody) =
        ( [ mkG (EHole qnName), mkG (EHole qnVar), mkG qnBody, mkG qnGuard, mkG qnDom, mkG qnOp, mkG qnExpr ]
        , \ xs -> let qnName'   = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 xs
                      qnVar'    = mapMaybe (\ j -> case j of EHole i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 1 xs
                      qnBody'   = fromGs $ take 1 $ drop 2 xs
                      qnGuard'  = fromGs $ take 1 $ drop 3 xs
                      -- qnDom'    = mapMaybe (\ j -> case j of D     i -> Just i; _ -> Nothing ) $ fromGs $ take 1 $ drop 3 xs
                      qnDom'    = fromGs $ take 1 $ drop 4 xs
                      qnOp'     = fromGs $ take 1 $ drop 5 xs
                      qnExpr'   = fromGs $ take 1 $ drop 6 xs
                  in  if length qnName'   == 1 &&
                         length qnVar'    == 1 &&
                         length qnBody'   == 1 &&
                         length qnDom'    == 1 &&
                         length qnOp'     == 1 &&
                         length qnExpr'   == 1 &&
                         length qnGuard'  == 1
                         then QuantifiedExpr (head qnName')
                                             (head qnVar')
                                             (Just (head qnDom'))
                                             (Just (head qnOp', head qnExpr'))
                                             (head qnGuard')
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
        qnGuard  <- optionMaybe (comma *> parse)
        qnBody   <- dot *> parse
        let
            f []     = error "The Impossible has happenned. in QuantifiedExpr.parse.f"
            f [i]    = QuantifiedExpr qnName i qnDom qnExpr (QuanGuard (maybeToList qnGuard)) qnBody
            f (i:is) = QuantifiedExpr qnName i qnDom qnExpr (QuanGuard []) (Q $ f is)
        return (f qnVars)
    pretty (QuantifiedExpr qnName qnVar qnDom qnExpr qnGuard qnBody)
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
        <>  ( case qnGuard of
                QuanGuard []  -> Pr.empty
                QuanGuard [i] -> Pr.comma <+> pretty i
                _   -> error "Multiple guards, what the hell?"
            )
        <+> Pr.dot
        Pr.$$ Pr.nest 4 (pretty qnBody)

instance TypeOf QuantifiedExpr where
    typeOf (QuantifiedExpr {quanName = Identifier qnName}) = do
        qd :: Maybe QuantifierDecl <- getBinding qnName
        case qd of
            Nothing -> throwError $ "Quantifier not declared:" <+> text qnName
            Just (QuantifierDecl _ _ identity) -> typeOf identity



newtype QuanGuard = QuanGuard [Expr]
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance NodeTag QuanGuard

instance Hole QuanGuard

instance GPlate QuanGuard where
    gplate (QuanGuard xs) = gplateUniList QuanGuard xs

instance MatchBind QuanGuard where
    match (QuanGuard [EHole (Identifier _)]) (QuanGuard []) = return ()
    match (QuanGuard [p]) (QuanGuard [a]) = match p a
    match _ _ = throwError "while pattern matching a QuanGuard"

    bind (QuanGuard []) = return Nothing
    bind (QuanGuard [EHole (Identifier nm)]) = do
        bindings <- get
        case M.lookup nm bindings of
            Nothing -> return Nothing -- $ Just $ QuanGuard []
            Just _  -> do
                res <- bind (EHole (Identifier nm))
                case res of
                    Nothing -> return Nothing
                    Just x  -> return $ Just $ QuanGuard [x]
    bind (QuanGuard [x]) = do
        res <- bind x
        case res of
            Nothing -> return Nothing -- $ Just $ QuanGuard []
            Just x' -> return $ Just $ QuanGuard [x']
    bind _ = error "Multiple guards, what the hell?"

instance ParsePrint QuanGuard where
    parse = error "QuanGuard parse"
    pretty = text . show

