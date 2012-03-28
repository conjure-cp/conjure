{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Language.Essence.Identifier where

import Control.Applicative
import Control.Monad ( liftM, msum )
import Control.Monad.Error ( throwError )
import Data.Generics ( Data )
import Data.List.Split ( splitOn )
-- import Data.Set as S ( member )
import Data.String ( IsString, fromString )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary, arbitrary, choose )

-- import Constants ( reservedSet )
import GenericOps.Core ( NodeTag, Hole, GPlate, MatchBind, getBinding, bottomUp )
import ParsecUtils ( identifier )
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>), text )

import {-# SOURCE #-} Language.Essence.Binding ( Binding(..) )
import {-# SOURCE #-} Language.Essence.Domain ( Domain, DomainOf, domainOf )
import {-# SOURCE #-} Language.Essence.Expr ( Expr )
import {-# SOURCE #-} Language.Essence.Type ( Type(..), TypeOf, typeOf )
import {-# SOURCE #-} Language.Essence.Value ( Value )



newtype Identifier = Identifier String
    deriving (Eq, Ord, Read, Show, Data, Typeable, Generic)

instance IsString Identifier where
    fromString = Identifier

instance NodeTag Identifier

instance Hole Identifier

instance GPlate Identifier -- always leaf. because String isn't a GPlate.

instance MatchBind Identifier

instance ParsePrint Identifier where
    parse = Identifier <$> identifier
    pretty (Identifier nm) = text nm

instance Arbitrary Identifier where
    arbitrary = Identifier . return <$> choose ('a', 'z')

instance TypeOf Identifier where
    typeOf p@(Identifier nm') = do
        -- bindings <- get
        -- return $ case M.lookup nm bindings of
        --     Nothing -> Nothing
        --     Just gr -> fromG gr
        let nm = head $ splitOn "#" nm'
        t :: Maybe Type    <- getBinding nm
        v :: Maybe Value   <- getBinding nm
        d :: Maybe Domain  <- getBinding nm
        x :: Maybe Expr    <- getBinding nm
        b :: Maybe Binding <- getBinding nm
        tt <- case t of Nothing -> return Nothing; Just i -> Just `liftM` typeOf i
        tv <- case v of Nothing -> return Nothing; Just i -> Just `liftM` typeOf i
        td <- case d of Nothing -> return Nothing; Just i -> Just `liftM` typeOf i
        tx <- case x of Nothing -> return Nothing; Just i -> Just `liftM` typeOf i
        tb <- case b of Just (Find _  dom) -> Just `liftM` typeOf dom
                        Just (Given _ dom) -> Just `liftM` typeOf dom
                        Just (LettingType _ ty@(TEnum (Just is)))
                            | p `elem` is  -> return $ Just ty
                        _                  -> return Nothing
        -- let msg = "typeOf Identifier" <+> braces (text nm') <+> text (show [tt,tv,td,tx,tb])
        case msum [tt,tv,td,tx,tb] of
            Nothing -> throwError $ "Identifier not bound:" <+> text nm
            Just r  -> return r

instance DomainOf Identifier where
    domainOf (Identifier nm') = do
        let nm = head $ splitOn "#" nm'
        v :: Maybe Value   <- getBinding nm
        d :: Maybe Domain  <- getBinding nm
        x :: Maybe Expr    <- getBinding nm
        b :: Maybe Binding <- getBinding nm
        dv <- case v of Nothing -> return Nothing; Just i -> Just `liftM` domainOf i
        dd <- case d of Nothing -> return Nothing; Just i -> Just `liftM` domainOf i
        dx <- case x of Nothing -> return Nothing; Just i -> Just `liftM` domainOf i
        db <- case b of Just (Find _  dom) -> Just `liftM` domainOf dom
                        Just (Given _ dom) -> Just `liftM` domainOf dom
                        _                  -> return Nothing
        -- let msg = "domainOf Identifier" <+> braces (text nm') <+> text (show [dv,dd,dx,db])
        case msum [dv,dd,dx,db] of
            Nothing -> throwError $ "Identifier not bound:" <+> text nm
            Just r  -> return r


scopeIdentifiers :: GPlate a => String -> a -> a
scopeIdentifiers prefix = bottomUp f
    where
        f p@(Identifier "_") = p
        -- f p@(Identifier nm ) | S.member nm reservedSet = p
        f p@(Identifier nm ) | nm `elem` ["forall","exists","sum","indices","refn","repr","domSize"] = p
        f   (Identifier nm ) = Identifier (prefix ++ nm)
