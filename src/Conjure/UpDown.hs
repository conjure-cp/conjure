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
import Data.List ( findIndex, transpose )

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
      , Text -> [Text]                              -- names down
      , [Text] -> m Text                            -- names up
      , Text -> Constant -> m [(Text,Constant)]     -- constant down
      , Text -> [(Text,Constant)] -> m Constant     -- constant up
      )

type UpDownType m = Domain Representation Constant -> UpDownResultType m
    
downDomain :: MonadError UpDownError m => Domain Representation Constant -> m [Domain Representation Constant]
downDomain domain = do
    (gen,_,_,_,_) <- upDown domain
    gen

downConstant :: MonadError UpDownError m => Domain Representation Constant -> Text -> Constant -> m [(Text, Constant)]
downConstant domain name constant = do
    (_,_,_,gen,_) <- upDown domain
    gen name constant

upConstant :: MonadError UpDownError m => Domain Representation Constant -> Text -> [(Text, Constant)] -> m Constant
upConstant domain name constants = do
    (_,_,_,_,gen) <- upDown domain
    gen name constants

-- | This is about one level.
--   Describes how, for a representation, we can translate a given high level domain into a low level domain.
--   And how, for that representation and a domain, we can translate a given constant of the high level domain into a constant of the low level domain.
--   And how, for that representation and a domain, we can translate a given constant of the low level domain into a constant of the high level domain.
upDown :: MonadError UpDownError m => UpDownType m

upDown d@(DomainBool   {}) = upDownNoOp d
upDown d@(DomainInt    {}) = upDownNoOp d
upDown (DomainMatrix index inner) = upDownMatrix index inner

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
upDownNoOp domain = return 
    ( return [domain]
    , singletonList
    , return . headNote "[Conjure.UpDown.upDownNoOp] nameUp"
    , \ name constant -> return [(name, constant)]
    , \ name ctxt ->
            case lookup name ctxt of
                Nothing -> throwError $ ConstantUpError $ sep
                                [ "upDownNoOp"
                                , "name  :" <+> pretty name
                                , "ctxt  :" <+> sep (map pretty ctxt)
                                ]
                Just c -> return c
    )

upDownEnum :: MonadError UpDownError m => UpDownType m
upDownEnum (DomainEnum defn@(DomainDefnEnum _name enums) ranges) = return
    ( liftM singletonList domainOut
    , nameDown
    , nameUp . headNote "[Conjure.UpDown.upDownEnum] nameUp"
    , \ name constant -> do constant' <- constantDown constant ; return [(name, constant')]
    , constantUp
    )

    where

        nameDown name = [ name `mappend` "_enum" ]

        nameUp n =
            case stripSuffix "_enum" n of
                Nothing -> throwError $ NameUpError $ "[Conjure.UpDown.upDownEnum] nameUp:" <+> pretty n
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

        constantUp name ctxt =
            case lookup name ctxt of
                Just (ConstantInt x) ->
                    case atMay enums (x - 1) of
                        Nothing -> throwError $ ConstantUpError $ "[Conjure.UpDown.upDownEnum] Integer constant out of range for enum:" <+> pretty x
                        Just y  -> return (ConstantEnum defn y)
                _ -> throwError $ ConstantUpError $ "[Conjure.UpDown.upDownEnum] constantUp:" <+> pretty name <+> sep (map pretty ctxt)

upDownEnum d = throwError $ RepresentationDoesntMatch $ "[Conjure.UpDown.upDownEnum] Only works on enum domains. this is not one:" <+> pretty d

upDownMatrix :: MonadError UpDownError m => Domain () Constant -> Domain Representation Constant -> UpDownResultType m
upDownMatrix index inner = do
    (innerDomainsDownGen, innerNamesDown, innerNamesUp, innerConstantsDown, innerConstantsUp) <- upDown inner
    innerDomainsDown <- innerDomainsDownGen
    return
        ( return [ DomainMatrix index i | i <- innerDomainsDown ]
        , error "namesDown"
        , error "namesUp"
        , \ name constant -> 
            case constant of
                ConstantMatrix _ cs -> do
                    foo <- liftM transpose $ sequence [ innerConstantsDown name c | c <- cs ]
                    let bar = [ (fst (head i), map snd i) | i <- foo ]
                    return [ (name, ConstantMatrix index cs') | (name, cs') <- bar ]
                _ -> throwError $ ConstantDownError $ "upDownMatrix"
        , \ name ctxt -> do
            let names' = innerNamesDown name
            forM names' $ \ name' -> do
                ctxt' <- constantUpMatrix name name' inner ctxt
                throwError $ ConstantUpError $ sep $
                            [ "upDownMatrix constantsUp Just"
                            , "name  :" <+> pretty name
                            , "ctxt  :" <+> sep (map pretty ctxt)
                            , "ctxt' :" <+> sep (map pretty ctxt')
                            ]
            --
            -- constants <- forM names' $ \ name' ->
            --     case lookup name' ctxt of
            --         Just (ConstantMatrix _ cs) -> do
            --             cs' <- innerConstantsUp name' ctxt
            --             throwError $ ConstantUpError $ sep $
            --                         [ "upDownMatrix constantsUp Just"
            --                         , "name  :" <+> pretty name
            --                         , "ctxt  :" <+> sep (map pretty ctxt)
            --                         , "names':" <+> sep (map pretty names')
            --                         , "cs    :" <+> sep (map pretty cs)
            --                         , "cs'   :" <+> pretty cs'
            --                         -- , "ctxt' :" <+> sep (map pretty ctxt')
            --                         ]
            --         _ ->
            --             throwError $ ConstantUpError $ sep $
            --                         [ "upDownMatrix constantsUp Nothing"
            --                         , "name  :" <+> pretty name
            --                         , "ctxt  :" <+> sep (map pretty ctxt)
            --                         , "names':" <+> sep (map pretty names')
            --                         -- , "ctxt' :" <+> sep (map pretty ctxt')
            --                         ]
            error "foo"
            -- return (ConstantMatrix index constants)
        )

-- type UpDownResultType m =
--     m ( m [Domain Representation Constant]          -- the low level domain
--       , Text -> [Text]                              -- names down
--       , [Text] -> m Text                            -- names up
--       , Text -> Constant -> m [(Text,Constant)]     -- constant down
--       , Text -> [(Text,Constant)] -> m Constant     -- constant up
--       )

-- constantUpMatrix :: MonadError UpDownError m => Text -> Text -> Domain Representation Constant -> [(Text, Constant)] -> m [(Text, Constant)]

upDownTuple :: MonadError UpDownError m => UpDownType m
upDownTuple (DomainTuple ds) = return
    ( return ds
    , namesDown
    , namesUp
    , constantsDown
    , constantsUp
    )

    where

        namesDown name =
            [ name `mappend` suffix
            | i <- [1 .. length ds]
            , let suffix = "_" `mappend` pack (show i)
            ]

        namesUp names = do
            allStripped <- sequence
                [ case stripSuffix suffix n of
                    Nothing -> throwError $ NameUpError $ "[Conjure.UpDown.upDownTuple] namesUp:" <+> pretty n
                    Just n' -> return n'
                | (n,i) <- zip names [1 .. length names]
                , let suffix = "_" `mappend` pack (show i)
                ]
            if length (nub allStripped) == 1
                then return (head allStripped)
                else throwError $ NameUpError $ "[Conjure.UpDown.upDownTuple] namesUp:" <+> pretty (show names)

        constantsDown name constant =
            case constant of
                ConstantTuple cs -> liftM concat $ sequence [ downConstant d n c | (n,d,c) <- zip3 (namesDown name) ds cs ]
                _ -> throwError $ ConstantDownError $ "[Conjure.UpDown.upDownTuple] constantsDown:" <+> pretty constant

        constantsUp _name ctxt = return (ConstantTuple (map snd ctxt))

upDownTuple d = throwError $ RepresentationDoesntMatch $ "[Conjure.UpDown.upDownTuple] Only works on tuple domains. this is not one:" <+> pretty d

upDownSetExplicit :: MonadError UpDownError m => Constant -> Domain Representation Constant -> UpDownResultType m
upDownSetExplicit size innerDomain = return
    ( return [domain]
    , singletonList . nameDown
    , nameUp . headNote "[Conjure.UpDown.upDownSetExplicit] nameUp:"
    , \ name constant -> do constant' <- constantDown constant ; return [(nameDown name, constant')]
    , constantUp
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

        constantUp name ctxt = do
            ctxt' <- constantUpMatrix name (nameDown name) innerDomain ctxt
            case lookup name ctxt' of
                Just (ConstantMatrix _ xs) -> return $ ConstantSet $ sort $ nub xs
                _ -> throwError $ ConstantUpError $ sep $
                        [ "[Conjure.UpDown.upDownSetExplicit] constantUp:"
                        , "name  :" <+> pretty name
                        , "ctxt  :" <+> sep (map pretty ctxt)
                        , "ctxt' :" <+> sep (map pretty ctxt')
                        ]

constantUpMatrix :: MonadError UpDownError m => Text -> Text -> Domain Representation Constant -> [(Text, Constant)] -> m [(Text, Constant)]
constantUpMatrix name name' domain ctxt = do
    (_,_,_,constantDown,_) <- upDown domain
    case lookup name' ctxt of
        Just (ConstantMatrix index vals) -> do
            vals' <- sequence [ constantDown name' val | val <- vals ]
            return $ (name, ConstantMatrix index (map (snd . head) vals')) : ctxt
        Just c  -> throwError $ ConstantUpError $ sep
                        [ "constantUpMatrix -- found, but not a matrix"
                        , "found :" <+> pretty c
                        , "name  :" <+> pretty name
                        , "ctxt  :" <+> sep (map pretty ctxt)
                        ]
        Nothing -> throwError $ ConstantUpError $ sep
                        [ "constantUpMatrix -- not found"
                        , "name  :" <+> pretty name
                        , "ctxt  :" <+> sep (map pretty ctxt)
                        ]


singletonList :: a -> [a]
singletonList = return

