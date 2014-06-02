{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.UpDown where

-- conjure
import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty

-- base
import Data.List ( findIndex )

-- text
import Data.Text ( pack, stripSuffix )

-- uniplate
import Data.Generics.Uniplate.Data ( transformBiM )

-- safe
import Safe ( atMay, headNote )


data UpDownError
    = NoRepresentationMatches Doc
    | RepresentationDoesntMatch Doc
    | NameDownError Doc
    | ConstantDownError Doc
    | ConstantUpError Doc
    deriving (Show)

instance Eq UpDownError where
    NoRepresentationMatches a == NoRepresentationMatches b = show a == show b
    RepresentationDoesntMatch a == RepresentationDoesntMatch b = show a == show b
    NameDownError a == NameDownError b = show a == show b
    ConstantDownError a == ConstantDownError b = show a == show b
    ConstantUpError a == ConstantUpError b = show a == show b
    _ == _ = False

type UpDownType m =
       Domain Constant
    -> m ( m [Domain Constant]              -- the low level domain
         , [Text -> Text]                   -- names down
         , [Text -> m Text]                 -- names up
         , Constant -> m [Constant]         -- constant down
         , [Constant] -> m Constant         -- constant up
         )
    
downDomain :: MonadError UpDownError m => Representation -> Domain Constant -> m [Domain Constant]
downDomain representation domain = do (gen,_,_,_,_) <- upDown representation domain ; gen

downConstant :: MonadError UpDownError m => Representation -> Domain Constant -> Constant -> m [Constant]
downConstant representation domain constant = do (_,_,_,gen,_) <- upDown representation domain ; gen constant

upConstant :: MonadError UpDownError m => Representation -> Domain Constant -> [Constant] -> m Constant
upConstant representation domain enums = do (_,_,_,_,gen) <- upDown representation domain ; gen enums

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
    return ( return [d]
           , singletonList id
           , singletonList return
           , return . singletonList
           , return . headNote "Conjure.UpDown.upDownNoOp"
           )

upDownEnum :: MonadError UpDownError m => UpDownType m
upDownEnum (DomainEnum defn@(DomainDefnEnum _name enums) ranges) =
    return ( liftM singletonList domainOut
           , singletonList nameDown
           , singletonList nameUp
           , (liftM . liftM) singletonList constantDown
           , constantUp . headNote "Conjure.UpDown.upDownEnum"
           )

    where

        nameDown = (`mappend` "_enum")

        nameUp n =
            case stripSuffix "_enum" n of
                Nothing -> throwError $ NameDownError $ "upDownEnum:" <+> pretty n
                Just n' -> return n'

        nbConstants = genericLength enums
        domainOut =
            if null ranges
                then return $ DomainInt [RangeBounded (ConstantInt 1) (ConstantInt nbConstants)]
                else DomainInt `liftM` transformBiM constantDown ranges

        constantDown v =
            case v of
                ConstantEnum _ x ->
                    case findIndex (x==) enums of
                        Nothing -> throwError $ ConstantDownError $ "This identifier isn't a member of the enum:" <+> pretty v
                        Just y  -> return $ ConstantInt (y + 1)
                _ -> throwError $ ConstantDownError $ "upDownEnum:" <+> pretty v

        constantUp v =
            case v of
                ConstantInt x ->
                    case atMay enums (x - 1) of
                        Nothing -> throwError $ ConstantUpError $ "Integer constant out of range for enum:" <+> pretty x
                        Just y  -> return (ConstantEnum defn y)
                _ -> throwError $ ConstantUpError $ "upDownEnum:" <+> pretty v

upDownEnum d = throwError $ RepresentationDoesntMatch $ "upDownEnum only works on enum domains. this is not one:" <+> pretty d

upDownTuple :: MonadError UpDownError m => UpDownType m
upDownTuple (DomainTuple ds) = return (return ds, namesDown, namesUp, constantsDown, constantsUp)

    where

        namesDown =
            [ (`mappend` suffix)
            | i <- [1 .. length ds]
            , let suffix = "_" `mappend` pack (show i)
            ]

        namesUp =
            [ \ n ->
                case stripSuffix suffix n of
                    Nothing -> throwError $ NameDownError $ "upDownTuple:" <+> pretty n
                    Just n' -> return n'
            | i <- [1 .. length ds]
            , let suffix = "_" `mappend` pack (show i)
            ]

        constantsDown v =
            case v of
                ConstantTuple xs -> return xs
                _ -> throwError $ ConstantDownError $ "upDownTuple.down:" <+> pretty v

        constantsUp vs = return (ConstantTuple vs)

upDownTuple d = throwError $ RepresentationDoesntMatch $ "upDownTuple only works on tuple domains. this is not one:" <+> pretty d



singletonList :: a -> [a]
singletonList = return

