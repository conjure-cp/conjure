{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.UpDown
    ( UpDownError(..)
    -- essential
    , upDown
    -- derived
    , downDomain, downConstant, upConstant
    ) where

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
    | NameUpError Doc
    | ConstantDownError Doc
    | ConstantUpError Doc
    deriving (Show)

instance Eq UpDownError where
    NoRepresentationMatches a == NoRepresentationMatches b = show a == show b
    RepresentationDoesntMatch a == RepresentationDoesntMatch b = show a == show b
    NameDownError a == NameDownError b = show a == show b
    NameUpError a == NameUpError b = show a == show b
    ConstantDownError a == ConstantDownError b = show a == show b
    ConstantUpError a == ConstantUpError b = show a == show b
    _ == _ = False

type UpDownResultType m =
    m ( m [Domain Representation Constant]          -- the low level domain
      , [Text -> Text]                              -- names down
      , [Text] -> m Text                            -- names up
      , Constant -> m [Constant]                    -- constant down
      , [Constant] -> m Constant                    -- constant up
      )

type UpDownType m = Domain Representation Constant -> UpDownResultType m
    
downDomain :: MonadError UpDownError m => Domain Representation Constant -> m [Domain Representation Constant]
downDomain domain = do (gen,_,_,_,_) <- upDown domain ; gen

downConstant :: MonadError UpDownError m => Domain Representation Constant -> Constant -> m [Constant]
downConstant domain constant = do (_,_,_,gen,_) <- upDown domain ; gen constant

upConstant :: MonadError UpDownError m => Domain Representation Constant -> [Constant] -> m Constant
upConstant domain enums = do (_,_,_,_,gen) <- upDown domain ; gen enums

-- | This is about one level.
--   Describes how, for a representation, we can translate a given high level domain into a low level domain.
--   And how, for that representation and a domain, we can translate a given constant of the high level domain into a constant of the low level domain.
--   And how, for that representation and a domain, we can translate a given constant of the low level domain into a constant of the high level domain.
upDown :: MonadError UpDownError m => UpDownType m

upDown d@(DomainBool   {}) = upDownNoOp d
upDown d@(DomainInt    {}) = upDownNoOp d
upDown d@(DomainEnum   {}) = upDownEnum d
upDown d@(DomainTuple  {}) = upDownTuple d

upDown (DomainSet (Representation "Explicit") (SetAttrSize size) innerDomain) = upDownSetExplicit size innerDomain

-- upDown (DomainMatrix Domain Domain)
-- upDown (DomainSet DomainAttributes Domain)
-- upDown (DomainMSet DomainAttributes Domain)
-- upDown (DomainFunction DomainAttributes Domain Domain)
-- upDown (DomainRelation DomainAttributes [Domain])
-- upDown (DomainPartition DomainAttributes Domain)
-- upDown (DomainOp Text [Domain])

upDown domain =
    throwError $ NoRepresentationMatches $ vcat [ "[Conjure.UpDown.upDown]"
                                                , pretty domain
                                                ]

upDownNoOp :: MonadError UpDownError m => UpDownType m
upDownNoOp d =
    return ( return [d]
           , singletonList id
           , return . headNote "[Conjure.UpDown.upDownNoOp] nameUp"
           , return . singletonList
           , return . headNote "[Conjure.UpDown.upDownNoOp] constantUp"
           )

upDownEnum :: MonadError UpDownError m => UpDownType m
upDownEnum (DomainEnum defn@(DomainDefnEnum _name enums) ranges) =
    return ( liftM singletonList domainOut
           , singletonList nameDown
           , nameUp . headNote "[Conjure.UpDown.upDownEnum] nameUp"
           , (liftM . liftM) singletonList constantDown
           , constantUp . headNote "[Conjure.UpDown.upDownEnum] constantUp"
           )

    where

        nameDown = (`mappend` "_enum")

        nameUp n =
            case stripSuffix "_enum" n of
                Nothing -> throwError $ NameDownError $ "[Conjure.UpDown.upDownEnum] nameUp:" <+> pretty n
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
                        Nothing -> throwError $ ConstantDownError $ "[Conjure.UpDown.upDownEnum] This identifier isn't a member of the enum:" <+> pretty v
                        Just y  -> return $ ConstantInt (y + 1)
                _ -> throwError $ ConstantDownError $ "[Conjure.UpDown.upDownEnum] constantDown:" <+> pretty v

        constantUp v =
            case v of
                ConstantInt x ->
                    case atMay enums (x - 1) of
                        Nothing -> throwError $ ConstantUpError $ "[Conjure.UpDown.upDownEnum] Integer constant out of range for enum:" <+> pretty x
                        Just y  -> return (ConstantEnum defn y)
                _ -> throwError $ ConstantUpError $ "[Conjure.UpDown.upDownEnum] constantUp:" <+> pretty v

upDownEnum d = throwError $ RepresentationDoesntMatch $ "[Conjure.UpDown.upDownEnum] Only works on enum domains. this is not one:" <+> pretty d

upDownTuple :: MonadError UpDownError m => UpDownType m
upDownTuple (DomainTuple ds) = return (return ds, namesDown, namesUp, constantsDown, constantsUp)

    where

        namesDown =
            [ (`mappend` suffix)
            | i <- [1 .. length ds]
            , let suffix = "_" `mappend` pack (show i)
            ]

        namesUp names = do
            allStripped <- sequence
                [ case stripSuffix suffix n of
                    Nothing -> throwError $ NameDownError $ "[Conjure.UpDown.upDownTuple] namesUp:" <+> pretty n
                    Just n' -> return n'
                | (n,i) <- zip names [1 .. length names]
                , let suffix = "_" `mappend` pack (show i)
                ]
            if length (nub allStripped) == 1
                then return (head allStripped)
                else throwError $ NameUpError $ "[Conjure.UpDown.upDownTuple] namesUp:" <+> pretty (show names)

        constantsDown v =
            case v of
                ConstantTuple xs -> return xs
                _ -> throwError $ ConstantDownError $ "[Conjure.UpDown.upDownTuple] constantsDown:" <+> pretty v

        constantsUp vs = return (ConstantTuple vs)

upDownTuple d = throwError $ RepresentationDoesntMatch $ "[Conjure.UpDown.upDownTuple] Only works on tuple domains. this is not one:" <+> pretty d

upDownSetExplicit :: MonadError UpDownError m => Constant -> Domain Representation Constant -> UpDownResultType m
upDownSetExplicit size innerDomain
    = return ( return [domain]
             , singletonList nameDown
             , nameUp . headNote "[Conjure.UpDown.upDownSetExplicit] nameUp:"
             , (liftM . liftM) singletonList constantDown
             , constantUp . headNote "[Conjure.UpDown.upDownSetExplicit] constantUp:"
             )

    where

        indexDomain = DomainInt [RangeBounded (ConstantInt 1) size]
        domain = DomainMatrix indexDomain innerDomain

        nameDown = (`mappend` "_Explicit")

        nameUp n =
            case stripSuffix "_Explicit" n of
                Nothing -> throwError $ NameUpError $ "[Conjure.UpDown.upDownSetExplicit] nameUp:" <+> pretty n
                Just n' -> return n'

        constantDown v =
            case v of
                ConstantSet xs -> return $ ConstantMatrix indexDomain xs
                _ -> throwError $ ConstantDownError $ "[Conjure.UpDown.upDownSetExplicit] constantDown:" <+> pretty v

        constantUp v =
            case v of
                ConstantMatrix _ xs -> return $ ConstantSet $ sort $ nub xs
                _ -> throwError $ ConstantUpError $ "[Conjure.UpDown.upDownSetExplicit] constantUp:" <+> pretty v



singletonList :: a -> [a]
singletonList = return

