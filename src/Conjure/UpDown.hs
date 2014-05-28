{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.UpDown where

import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty

import Data.Generics.Uniplate.Data ( transformBiM )
import Data.List ( findIndex )
import Safe ( atMay, headNote )


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


downDomain :: MonadError UpDownError m => Representation -> Domain -> m [Domain]
downDomain representation domain = do (gen,_,_) <- upDown representation domain ; gen

downValue :: MonadError UpDownError m => Representation -> Domain -> Value -> m [Value]
downValue representation domain value = do (_,gen,_) <- upDown representation domain ; gen value

upValue :: MonadError UpDownError m => Representation -> Domain -> [Value] -> m Value
upValue representation domain values = do (_,_,gen) <- upDown representation domain ; gen values

-- | This is about one level.
--   Describes how, for a representation, we can translate a given high level domain into a low level domain.
--   And how, for that representation and a domain, we can translate a given value of the high level domain into a value of the low level domain.
--   And how, for that representation and a domain, we can translate a given value of the low level domain into a value of the high level domain.
upDown
    :: MonadError UpDownError m
    => Representation
    -> Domain
    -> m ( m [Domain]                 -- the low level domain
         , Value -> m [Value]         -- value down
         , [Value] -> m Value         -- value up
         )
upDown NoRepresentation d@(DomainBool   {}) = upDownNoOp d
upDown NoRepresentation d@(DomainInt    {}) = upDownNoOp d
upDown NoRepresentation d@(DomainEnum   {}) = upDownEnum d
upDown NoRepresentation d@(DomainTuple  {}) = throwError $ NoRepresentationMatches $ vcat [ "tuples should be handled separately.", pretty d ]

-- upDown (DomainMatrix Domain Domain)
-- upDown (DomainSet DomainAttributes Domain)
-- upDown (DomainMSet DomainAttributes Domain)
-- upDown (DomainFunction DomainAttributes Domain Domain)
-- upDown (DomainRelation DomainAttributes [Domain])
-- upDown (DomainPartition DomainAttributes Domain)
-- upDown (DomainOp Text [Domain])

upDown representation domain =
    throwError $ NoRepresentationMatches $ vcat [ "bug in upDown"
                                                , pretty domain
                                                , pretty representation
                                                ]

upDownNoOp :: MonadError UpDownError m => Domain -> m (m [Domain], Value -> m [Value], [Value] -> m Value)
upDownNoOp d =
    return ( return [d]
           , return . singletonList
           , return . headNote "Conjure.UpDown.upDownNoOp"
           )

upDownEnum :: MonadError UpDownError m => Domain -> m (m [Domain], Value -> m [Value], [Value] -> m Value)
upDownEnum (DomainEnum (DomainDefnEnum _name values) ranges) =
    return ( liftM singletonList domainOut
           , (liftM . liftM) singletonList down
           , up . headNote "Conjure.UpDown.upDownEnum"
           )

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

singletonList :: a -> [a]
singletonList = return

