{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.UpDown where

import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty
import Language.E.TH

import Data.Generics.Uniplate.Data ( transformBiM )
import Data.List ( findIndex )
import Safe ( atMay )
import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck ( NonNegative(..), verbose, (==>) )


data UpDownError
    = NoRepresentationMatches Doc
    | RepresentationDoesntMatch Doc
    | ValueDownError Doc
    | ValueUpError Doc
    deriving (Show)

instance Eq UpDownError where
    NoRepresentationMatches a == NoRepresentationMatches b = show a == show b
    RepresentationDoesntMatch a == RepresentationDoesntMatch b = show a == show b
    ValueDownError a == ValueDownError b = show a == show b
    ValueUpError a == ValueUpError b = show a == show b
    _ == _ = False


downDomain :: MonadError UpDownError m => Representation -> Domain -> m Domain
downDomain representation domain = do (gen,_,_) <- upDown representation domain ; gen

downValue :: MonadError UpDownError m => Representation -> Domain -> Value -> m Value
downValue representation domain value = do (_,gen,_) <- upDown representation domain ; gen value

upValue :: MonadError UpDownError m => Representation -> Domain -> Value -> m Value
upValue representation domain value = do (_,_,gen) <- upDown representation domain ; gen value

upDown
    :: MonadError UpDownError m
    => Representation
    -> Domain
    -> m ( m Domain                 -- the low level domain
         , Value -> m Value         -- value down
         , Value -> m Value         -- value up
         )
upDown NoRepresentation d@(DomainBool{}) = upDownNoOp d
upDown NoRepresentation d@(DomainInt {}) = upDownNoOp d
upDown NoRepresentation d@(DomainEnum{}) = upDownEnum d

-- upDown (DomainTuple [Domain])
-- 
-- upDown (DomainMatrix Domain Domain)
-- 
-- upDown (DomainSet DomainAttributes Domain)
-- 
-- upDown (DomainMSet DomainAttributes Domain)
-- 
-- upDown (DomainFunction DomainAttributes Domain Domain)
-- 
-- upDown (DomainRelation DomainAttributes [Domain])
-- 
-- upDown (DomainPartition DomainAttributes Domain)
-- 
-- upDown (DomainOp Text [Domain])
-- 
-- upDown (DomainHack E          -- this is an ugly hack to be able to use expressions as domains. will go away later.)
-- 

upDown representation domain =
    throwError $ NoRepresentationMatches $ vcat [ "bug in upDown"
                                                , pretty domain
                                                , pretty representation
                                                ]

upDownNoOp :: MonadError UpDownError m => Domain -> m (m Domain, Value -> m Value, Value -> m Value)
upDownNoOp d = return (return d, \ v -> return v, \ v -> return v)

upDownEnum :: MonadError UpDownError m => Domain -> m (m Domain, Value -> m Value, Value -> m Value)
upDownEnum (DomainEnum (DomainDefnEnum _name values) ranges) = return (domainOut, down, up)

    where

        nbValues = genericLength values
        domainOut =
            if null ranges
                then return $ DomainInt [RangeBounded [xMake| value.literal := [Prim (I 1)] |]
                                                      [xMake| value.literal := [Prim (I nbValues)] |]]
                else DomainInt `liftM` transformBiM down ranges

        down v =
            case v of
                [xMatch| [Prim (S x)] := reference |] ->
                    case findIndex (x==) values of
                        Nothing -> throwError $ ValueDownError $ "This identifier isn't a member of the enum:" <+> pretty v
                        Just y  -> return [xMake| value.literal := [Prim (I (fromIntegral y + 1))] |]
                _ -> throwError $ ValueDownError $ "upDownEnum.down:" <+> pretty v

        up v =
            case v of
                [xMatch| [Prim (I x)] := value.literal |] ->
                    case atMay values (fromIntegral x - 1) of
                        Nothing -> throwError $ ValueUpError $ "Integer value out of range for enum:" <+> pretty x
                        Just y  -> return [xMake| reference := [Prim (S y)] |]
                _ -> throwError $ ValueUpError $ "upDownEnum.up:" <+> pretty v

upDownEnum d = throwError $ RepresentationDoesntMatch $ "upDownEnum only works on enum domains. this is not one:" <+> pretty d




pr :: (Pretty a, MonadTrans t, Monad (t IO)) => t IO a -> t IO ()
pr c = c >>= lift . print . pretty

test :: IO ()
test = hspec $ do
    describe "enum up-down" $ do
        let enumValues = ["apple", "orange", "peach", "melon"]
        let enumDomainDefn = DomainDefnEnum "fruits" enumValues
        let enumDomain = DomainEnum enumDomainDefn []
        let intDomain = DomainInt [RangeBounded [eMake| 1 |] [eMake| 4 |]]
        it "enum domains should be turned into int domains" $ do
            downDomain NoRepresentation enumDomain `shouldBe` Right intDomain

        it "enum values should be turned into int values, and back" $ do

            downValue NoRepresentation enumDomain [eMake| apple |] `shouldBe` Right [eMake| 1 |]
            downValue NoRepresentation enumDomain [eMake| peach |] `shouldBe` Right [eMake| 3 |]
            downValue NoRepresentation enumDomain [eMake| plum  |] `shouldBe` Left (ValueDownError "This identifier isn't a member of the enum: plum")

            upValue NoRepresentation enumDomain [eMake| 1 |] `shouldBe` Right [eMake| apple |]
            upValue NoRepresentation enumDomain [eMake| 3 |] `shouldBe` Right [eMake| peach |]
            upValue NoRepresentation enumDomain [eMake| 0 |] `shouldBe` Left (ValueUpError "Integer value out of range for enum: 0")

        it "enum down&up with quickcheck" $ property $ verbose $
            let downAndUp = downValue NoRepresentation enumDomain >=> upValue NoRepresentation enumDomain
            in  \ (NonNegative i) -> i < length enumValues ==>
                    let e = [xMake| reference := [Prim (S (enumValues !! i))] |]
                    in  downAndUp e == Right e

        -- 
        -- it 
        -- result <- runExceptT $ do
        --     (domainGen, down, up) <- upDown NoRepresentation enumDomain
        --     intDomain <- domainGen
        --     pr domainGen
        --      down [eMake| apple |]
        --     pr $ down [eMake| peach |]
        --     pr $ up [eMake| 1 |]
        --     pr $ up [eMake| 4 |]
        --     pr $ down [eMake| blah |]
        -- case result of
        --     Right () -> return ()
        --     Left err -> do
        --         putStrLn " !! There was an error !!"
        --         print err





