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
import Data.Map ( elems )
import Data.Maybe ( mapMaybe )
import Data.String ( IsString, fromString )
import Data.Typeable ( Typeable )
import GHC.Generics ( Generic )
import Test.QuickCheck ( Arbitrary, arbitrary, choose )

import Constants ( trace )
import Has ( getM )
import GenericOps.Core ( NodeTag
                       , Hole
                       , GPlate, bottomUp, fromGs
                       , MatchBind, getBinding, BindingsMap )
import ParsecUtils ( identifier )
import ParsePrint ( ParsePrint, parse, pretty )
import PrintUtils ( (<+>), text )

import {-# SOURCE #-} Language.Essence.Binding ( Binding(..) )
import {-# SOURCE #-} Language.Essence.Domain ( Domain, DomainOf, domainOf )
import {-# SOURCE #-} Language.Essence.Expr ( Expr )
import                Language.Essence.Lambda ( Lambda )
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
    typeOf p | trace ("typeOf Identifier: " ++ show (pretty p)) False = undefined
    typeOf (Identifier nm') = do
        -- bindings <- get
        -- return $ case M.lookup nm bindings of
        --     Nothing -> Nothing
        --     Just gr -> fromG gr
        let nm = head $ splitOn "#" nm'
        t :: Maybe Type    <- getBinding nm

        v :: Maybe Value   <- getBinding nm
        tv <- maybe (return Nothing) (liftM Just . typeOf) v

        d :: Maybe Domain  <- getBinding nm
        td <- maybe (return Nothing) (liftM Just . typeOf) d

        x :: Maybe Expr    <- getBinding nm
        tx <- maybe (return Nothing) (liftM Just . typeOf) x

        b :: Maybe Binding <- getBinding nm
        tb <- maybe (return Nothing) (liftM Just . typeOf) b

        l :: Maybe Lambda  <- getBinding nm
        tl <- case l of Nothing -> return Nothing; Just i -> Just `liftM` typeOf i

        -- let msg = "typeOf Identifier" <+> text nm' <+> text (show [tt,tv,td,tx,tb])
        case msum [t,tv,td,tx,tb,tl] of
            -- Nothing -> throwError $ "Identifier not bound:" <+> text nm
            Just r  -> return r
            Nothing -> do -- checking if this is a reference to an enum type. useful in iconjure.
                st :: BindingsMap <- getM
                let rs = flip mapMaybe (fromGs (elems st)) $ \ j -> case j of
                            LettingType ty (TEnum (Just is)) | Identifier nm `elem` is -> Just (THole ty)
                            _ -> Nothing
                case rs of
                    [j] -> return j
                    _   -> throwError $ "Identifier not bound:" <+> text nm


instance DomainOf Identifier where
    domainOf (Identifier nm') = do
        let nm = head $ splitOn "#" nm'
        v :: Maybe Value   <- getBinding nm
        d :: Maybe Domain  <- getBinding nm
        x :: Maybe Expr    <- getBinding nm
        b :: Maybe Binding <- getBinding nm
        dv <- maybe (return Nothing) (liftM Just . domainOf) v
        dd <- maybe (return Nothing) (liftM Just . domainOf) d
        dx <- maybe (return Nothing) (liftM Just . domainOf) x
        db <- case b of Just (Find _  dom) -> Just `liftM` domainOf dom
                        Just (Given _ dom) -> Just `liftM` domainOf dom
                        _                  -> return Nothing
        -- let msg = "domainOf Identifier" <+> text nm' <+> text (show [dv,dd,dx,db])
        case msum [dv,dd,dx,db] of
            Nothing -> throwError $ "Identifier not bound:" <+> text nm
            Just r  -> return r


scopeIdentifiers :: GPlate a => (String -> String) -> a -> a
scopeIdentifiers prefix = bottomUp f
    where
        f p@(Identifier "_") = p
        -- f p@(Identifier nm ) | S.member nm reservedSet = p
        f p@(Identifier nm ) | nm `elem` [ "forAll", "exists", "sum"
                                         , "indices", "refn", "repr", "domSize"
                                         , "glueOp", "guardOp"
                                         ] = p
        f   (Identifier nm ) = Identifier (prefix nm)


-- rename a single identifier.
identifierRenamer :: String -> String -> Identifier -> Identifier
identifierRenamer oldName newName (Identifier nm) | oldName == nm = Identifier newName
identifierRenamer _ _ p = p
