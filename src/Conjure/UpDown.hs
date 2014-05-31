{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.UpDown where

import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty

import Data.Text ( pack )
import Data.Generics.Uniplate.Data ( transformBiM )
import Data.List ( findIndex )
import Safe ( atMay, headNote )


data UpDownError
    = NoRepresentationMatches Doc
    | RepresentationDoesntMatch Doc
    | ConstantDownError Doc
    | ConstantUpError Doc
    deriving (Show)

instance Eq UpDownError where
    NoRepresentationMatches a == NoRepresentationMatches b = show a == show b
    RepresentationDoesntMatch a == RepresentationDoesntMatch b = show a == show b
    ConstantDownError a == ConstantDownError b = show a == show b
    ConstantUpError a == ConstantUpError b = show a == show b
    _ == _ = False

type UpDownType m =
       Domain Constant
    -> m ( [Text -> Text]                   -- name modifiers for the generated stuff
         , m [Domain Constant]                       -- the low level domain
         , Constant -> m [Constant]         -- constant down
         , [Constant] -> m Constant         -- constant up
         )
    
downDomain :: MonadError UpDownError m => Representation -> Domain Constant -> m [Domain Constant]
downDomain representation domain = do (_,gen,_,_) <- upDown representation domain ; gen

downConstant :: MonadError UpDownError m => Representation -> Domain Constant -> Constant -> m [Constant]
downConstant representation domain constant = do (_,_,gen,_) <- upDown representation domain ; gen constant

upConstant :: MonadError UpDownError m => Representation -> Domain Constant -> [Constant] -> m Constant
upConstant representation domain enums = do (_,_,_,gen) <- upDown representation domain ; gen enums

-- | This is about one level.
--   Describes how, for a representation, we can translate a given high level domain into a low level domain.
--   And how, for that representation and a domain, we can translate a given constant of the high level domain into a constant of the low level domain.
--   And how, for that representation and a domain, we can translate a given constant of the low level domain into a constant of the high level domain.
upDown :: MonadError UpDownError m => Representation -> UpDownType m
upDown NoRepresentation d@(DomainBool   {}) = upDownNoOp d
upDown NoRepresentation d@(DomainInt    {}) = upDownNoOp d
upDown NoRepresentation d@(DomainEnum   {}) = upDownEnum d
upDown NoRepresentation d@(DomainTuple  {}) = upDownTuple d

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

upDownNoOp :: MonadError UpDownError m => UpDownType m
upDownNoOp d =
    return ( singletonList id
           , return [d]
           , return . singletonList
           , return . headNote "Conjure.UpDown.upDownNoOp"
           )

upDownEnum :: MonadError UpDownError m => UpDownType m
upDownEnum (DomainEnum defn@(DomainDefnEnum _name enums) ranges) =
    return ( names
           , liftM singletonList domainOut
           , (liftM . liftM) singletonList down
           , up . headNote "Conjure.UpDown.upDownEnum"
           )

    where

        names = singletonList (`mappend` "_enum")

        nbConstants = genericLength enums
        domainOut =
            if null ranges
                then return $ DomainInt [RangeBounded (ConstantInt 1) (ConstantInt nbConstants)]
                else DomainInt `liftM` transformBiM down ranges

        down v =
            case v of
                ConstantEnum _ x ->
                    case findIndex (x==) enums of
                        Nothing -> throwError $ ConstantDownError $ "This identifier isn't a member of the enum:" <+> pretty v
                        Just y  -> return $ ConstantInt (y + 1)
                _ -> throwError $ ConstantDownError $ "upDownEnum.down:" <+> pretty v

        up v =
            case v of
                ConstantInt x ->
                    case atMay enums (x - 1) of
                        Nothing -> throwError $ ConstantUpError $ "Integer constant out of range for enum:" <+> pretty x
                        Just y  -> return (ConstantEnum defn y)
                _ -> throwError $ ConstantUpError $ "upDownEnum.up:" <+> pretty v

upDownEnum d = throwError $ RepresentationDoesntMatch $ "upDownEnum only works on enum domains. this is not one:" <+> pretty d

upDownTuple :: MonadError UpDownError m => UpDownType m
upDownTuple (DomainTuple ds) = return (names, return ds, down, up)

    where

        names = [ \ n -> mconcat [n, "_", pack (show i) ]
                | i <- [1 .. length ds]
                ]

        down v =
            case v of
                ConstantTuple xs -> return xs
                _ -> throwError $ ConstantDownError $ "upDownTuple.down:" <+> pretty v

        up vs = return (ConstantTuple vs)

upDownTuple d = throwError $ RepresentationDoesntMatch $ "upDownTuple only works on tuple domains. this is not one:" <+> pretty d



singletonList :: a -> [a]
singletonList = return

