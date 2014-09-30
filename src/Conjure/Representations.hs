{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE Rank2Types #-}

module Conjure.Representations
    ( down_, down, up
    , down1_, down1, up1
    , dispatch, reprOptions
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.Language.DomainSize ( valuesInIntDomain )
import Conjure.Language.ZeroVal ( zeroVal )

-- text
import Data.Text ( pack )


type DomainX x = Domain HasRepresentation x
type DomainC = Domain HasRepresentation Constant

-- the rDown_, rDown, and rUp all work one level at a time.
-- the maybe for rDown_ and rDown is Nothing when representation doesn't change anything.
-- like for primitives.
data Representation m = Representation
    { rCheck :: forall x r . (Pretty x, ExpressionLike x)
             => (Domain r x -> [Domain HasRepresentation x])        -- other checkers for inner domains
             -> Domain r x                                          -- this domain
             -> [Domain HasRepresentation x]                        -- with all repr options
    , rDown_ :: forall x . (Pretty x, ExpressionLike x)
             => (Name, DomainX x)                     -> m (Maybe [(Name, DomainX x)])
    , rDown  :: (Name, DomainC, Constant)             -> m (Maybe [(Name, DomainC, Constant)])
    , rUp    :: [(Name, Constant)] -> (Name, DomainC) -> m (Name, Constant)
    }
    

-- | refine a domain, one level.
--   the domain is allowed to be at the class level.
--   the trailing underscore signals that.
down1_
    :: (Applicative m, MonadError Doc m, Pretty x, ExpressionLike x)
    => Representation m
    ->           (Name, DomainX x)
    -> m (Maybe [(Name, DomainX x)])
down1_ = rDown_

-- | refine a domain, together with a constant, one level.
--   the domain has to be fully instantiated.
down1
    :: (Applicative m, MonadError Doc m)
    => Representation m
    ->           (Name, DomainC, Constant)
    -> m (Maybe [(Name, DomainC, Constant)])
down1 = rDown

-- | translate a bunch of low level constants up, one level.
--   the high level domain (i.e. the target domain) has to be given.
--   the domain has to be fully instantiated.
up1
    :: (Applicative m, MonadError Doc m)
    => Representation m
    ->   (Name, DomainC)
    ->  [(Name, Constant)]
    -> m (Name, Constant)
up1 repr p ctxt = rUp repr ctxt p


-- | refine a domain, all the way.
--   the domain is allowed to be at the class level.
--   the trailing underscore signals that.
down_
    :: (Applicative m, MonadError Doc m, Pretty x, ExpressionLike x)
    =>    (Name, DomainX x)
    -> m [(Name, DomainX x)]
down_ inp@(_, domain) = do
    mout <- down1_ (dispatch domain) inp
    case mout of
        Nothing -> return [inp]
        Just outs -> liftM concat $ mapM down_ outs

-- | refine a domain, together with a constant, all the way.
--   the domain has to be fully instantiated.
down
    :: (Applicative m, MonadError Doc m)
    =>    (Name, DomainC, Constant)
    -> m [(Name, DomainC, Constant)]
down inp@(_, domain, _) = do
    mout <- down1 (dispatch domain) inp
    case mout of
        Nothing -> return [inp]
        Just outs -> liftM concat $ mapM down outs

-- | translate a bunch of low level constants up, all the way.
--   the high level domain (i.e. the target domain) has to be given.
--   the domain has to be fully instantiated.
up
    :: (Applicative m, MonadError Doc m)
    =>  [(Name, Constant)]
    ->   (Name, DomainC)
    -> m (Name, Constant)
up ctxt (name, highDomain) = do
    toDescend'
        -- :: Maybe [(Name, DomainX x)]
        <- down1_ (dispatch highDomain) (name, highDomain)
    case toDescend' of
        Nothing ->
            case lookup name ctxt of
                Nothing -> throwError $ vcat
                    $ ("No value for:" <+> pretty name)
                    : "Bindings in context:"
                    : prettyContext ctxt
                Just val -> return (name, val)
        Just toDescend -> do
            midConstants
                 :: [(Name, Constant)]
                 <- sequence [ up ctxt (n,d) | (n,d) <- toDescend ]
            up1 (dispatch highDomain) (name, highDomain) midConstants


-- | Combine all known representations into one.
--   Dispatch into the actual implementation of the representation depending on the provided domain.
dispatch :: (Applicative m, MonadError Doc m, Pretty x) => Domain HasRepresentation x -> Representation m
dispatch domain =
    case domain of
        DomainBool{}    -> primitive
        DomainInt{}     -> primitive
        DomainTuple{}   -> tuple
        DomainMatrix{}  -> matrix
        DomainSet r _ _ ->
            case r of
                "Occurrence"                -> setOccurrence
                "Explicit"                  -> setExplicit
                "ExplicitVarSizeWithMarker" -> setExplicitVarSizeWithMarker
                "ExplicitVarSizeWithFlags"  -> setExplicitVarSizeWithFlags
                _ -> bug $ "No representation for the domain:" <+> pretty domain
        _ -> bug $ "No representation for the domain:" <+> pretty domain


-- | A list of all representations.
allReprs :: [Representation (Either Doc)]
allReprs =
    [ primitive, tuple, matrix
    , setOccurrence, setExplicit, setExplicitVarSizeWithMarker, setExplicitVarSizeWithFlags
    ]


-- | For a domain, produce a list of domains with different representation options.
--   This function should never return an empty list.
reprOptions :: (Pretty x, ExpressionLike x) => Domain r x -> [Domain HasRepresentation x]
reprOptions domain = concat [ rCheck r reprOptions domain | r <- allReprs ]


primitive :: MonadError Doc m => Representation m
primitive = Representation
    { rCheck = \ _ domain ->
        case domain of
            DomainBool -> [DomainBool]
            DomainInt rs -> [DomainInt rs]
            _ -> []
    , rDown_ = const $ return Nothing
    , rDown  = const $ return Nothing
    , rUp    = \ ctxt (name, _) ->
        case lookup name ctxt of
            Nothing -> throwError $ vcat
                $ ("No value for:" <+> pretty name)
                : "Bindings in context:"
                : prettyContext ctxt
            Just c  -> return (name, c)
    }


tuple :: MonadError Doc m => Representation m
tuple = Representation chck tupleDown_ tupleDown tupleUp

    where

        chck f (DomainTuple ds) = DomainTuple <$> mapM f ds
        chck _ _ = []

        mkName name i = mconcat [name, "_", Name (pack (show (i :: Int)))]

        tupleDown_ (name, DomainTuple ds) = return $ Just
            [ (mkName name i, d)
            | i <- [1..]
            | d <- ds
            ]
        tupleDown_ _ = throwError "N/A {tupleDown_}"

        -- TODO: check if (length ds == length cs)
        tupleDown (name, DomainTuple ds, ConstantTuple cs) = return $ Just
            [ (mkName name i, d, c)
            | i <- [1..]
            | d <- ds
            | c <- cs
            ]
        tupleDown _ = throwError "N/A {tupleDown}"

        tupleUp ctxt (name, DomainTuple ds) = do
            let names = map (mkName name) [1 .. length ds]
            vals <- forM names $ \ n ->
                case lookup n ctxt of
                    Nothing -> throwError $ vcat $
                        [ "No value for:" <+> pretty n
                        , "When working on:" <+> pretty name
                        , "With domain:" <+> pretty (DomainTuple ds)
                        ] ++
                        ("Bindings in context:" : prettyContext ctxt)
                    Just val -> return val
            -- TODO: check if (length ds == length vals)
            return (name, ConstantTuple vals)
        tupleUp _ _ = throwError "N/A {tupleUp}"


matrix :: (Applicative m, MonadError Doc m) => Representation m
matrix = Representation chck matrixDown_ matrixDown matrixUp

    where

        chck f (DomainMatrix indexDomain innerDomain) = DomainMatrix indexDomain <$> f innerDomain
        chck _ _ = []

        matrixDown_ (name, DomainMatrix indexDomain innerDomain) = do
            mres <- down1_ (dispatch innerDomain) (name, innerDomain)
            case mres of
                Nothing -> return Nothing
                Just mids -> return $ Just [ (n, DomainMatrix indexDomain d) | (n, d) <- mids ]
        matrixDown_ _ = throwError "N/A {matrixDown_}"

        -- TODO: check if indices are the same
        matrixDown (name, DomainMatrix indexDomain innerDomain, ConstantMatrix _indexDomain2 constants) = do
            mids1
                :: [Maybe [(Name, DomainC, Constant)]]
                <- sequence [ down1 (dispatch innerDomain) (name, innerDomain, c) | c <- constants ]
            let mids2 = catMaybes mids1
            if null mids2                                       -- if all were `Nothing`s
                then return Nothing
                else
                    if length mids2 == length mids1             -- if all were `Just`s
                        then do
                            let
                                mids3 :: [(Name, DomainC, [Constant])]
                                mids3 = [ ( head [ n | (n,_,_) <- line ]
                                          , head [ d | (_,d,_) <- line ]
                                          ,      [ c | (_,_,c) <- line ]
                                          )
                                        | line <- transpose mids2
                                        ]
                            return $ Just
                                [ ( n
                                  , DomainMatrix indexDomain d
                                  , ConstantMatrix indexDomain cs
                                  )
                                | (n, d, cs) <- mids3
                                ]
                        else
                            throwError $ vcat
                                [ "This is weird. Heterogeneous matrix literal?"
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                                ]
        matrixDown _ = throwError "N/A {matrixDown}"

        matrixUp ctxt (name, DomainMatrix indexDomain innerDomain)= do

            mid1
                :: Maybe [(Name, DomainC)]
                <- down1_ (dispatch innerDomain) (name, innerDomain)

            case mid1 of
                Nothing ->
                    -- the inner domain doesn't require refinement
                    -- there needs to be a binding with "name"
                    -- and we just pass it through
                    case lookup name ctxt of
                        Nothing -> throwError $ vcat $
                            [ "No value for:" <+> pretty name
                            , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                            ] ++
                            ("Bindings in context:" : prettyContext ctxt)
                        Just constant -> return (name, constant)
                Just mid2 -> do
                    -- the inner domain needs refinement
                    -- there needs to be bindings for each name in (map fst mid2)
                    -- we find those bindings, call (up1 name inner) on them, then lift
                    mid3
                        :: [(Name, [Constant])]
                        <- forM mid2 $ \ (n, _) ->
                            case lookup n ctxt of
                                Nothing -> throwError $ vcat $
                                    [ "No value for:" <+> pretty n
                                    , "When working on:" <+> pretty name
                                    , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                                    ] ++
                                    ("Bindings in context:" : prettyContext ctxt)
                                Just constant ->
                                    -- this constant is a ConstantMatrix, containing one component of the things to go into up1
                                    case constant of
                                        ConstantMatrix _ c -> return (n, c)
                                        _ -> throwError $ vcat
                                            [ "Expecting a matrix literal for:" <+> pretty n
                                            , "But got:" <+> pretty constant
                                            , "When working on:" <+> pretty name
                                            , "With domain:" <+> pretty (DomainMatrix indexDomain innerDomain)
                                            ]

                    let midNames     = map fst mid3
                    let midConstants = map snd mid3

                    mid4
                        :: [(Name, Constant)]
                        <- sequence
                            [ up1 (dispatch innerDomain) (name, innerDomain) (zip midNames cs)
                            | cs <- transpose midConstants
                            ]
                    let values = map snd mid4
                    return (name, ConstantMatrix indexDomain values)
        matrixUp _ _ = throwError "N/A {matrixUp}"


setOccurrence :: (Applicative m, MonadError Doc m) => Representation m
setOccurrence = Representation chck setDown_ setDown setUp

    where

        chck f (DomainSet _ attrs innerDomain@(DomainInt{})) = DomainSet "Occurrence" attrs <$> f innerDomain
        chck _ _ = []

        outName name = mconcat [name, "_", "Occurrence"]

        setDown_ (name, DomainSet "Occurrence" _attrs innerDomain@DomainInt{}) =
            return $ Just
                [ ( outName name
                  , DomainMatrix (forgetRepr innerDomain) DomainBool
                  )
                ]
        setDown_ _ = throwError "N/A {setDown_}"

        setDown (name, DomainSet "Occurrence" _attrs innerDomain@(DomainInt intRanges), ConstantSet constants) = do
                innerDomainVals <- valuesInIntDomain intRanges
                return $ Just
                    [ ( outName name
                      , DomainMatrix   (forgetRepr innerDomain) DomainBool
                      , ConstantMatrix (forgetRepr innerDomain)
                          [ ConstantBool isIn
                          | v <- innerDomainVals
                          , let isIn = ConstantInt v `elem` constants
                          ]
                      )
                    ]
        setDown _ = throwError "N/A {setDown}"

        setUp ctxt (name, domain@(DomainSet _ _ (DomainInt intRanges)))=
            case lookup (outName name) ctxt of
                Just constantMatrix ->
                    case constantMatrix of
                        ConstantMatrix _ vals -> do
                            innerDomainVals <- valuesInIntDomain intRanges
                            return (name, ConstantSet
                                            [ ConstantInt v
                                            | (v,b) <- zip innerDomainVals vals
                                            , b == ConstantBool True
                                            ] )
                        _ -> throwError $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (outName name)
                                , "But got:" <+> pretty constantMatrix
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                Nothing -> throwError $ vcat $
                    [ "No value for:" <+> pretty (outName name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
        setUp _ _ = throwError "N/A {setUp}"


setExplicit :: MonadError Doc m => Representation m
setExplicit = Representation chck setDown_ setDown setUp

    where

        chck f (DomainSet _ attrs@(SetAttrSize{}) innerDomain) = DomainSet "Explicit" attrs <$> f innerDomain
        chck _ _ = []

        outName name = mconcat [name, "_", "Explicit"]

        setDown_ (name, DomainSet "Explicit" (SetAttrSize size) innerDomain)
            = return $ Just
                [ ( outName name
                  , DomainMatrix
                      (DomainInt [RangeBounded (fromInt 1) size])
                      innerDomain
                  ) ]
        setDown_ _ = throwError "N/A {setDown_}"

        setDown (name, DomainSet "Explicit" (SetAttrSize size) innerDomain, ConstantSet constants) =
            let outIndexDomain = DomainInt [RangeBounded (ConstantInt 1) size]
            in  return $ Just
                    [ ( outName name
                      , DomainMatrix   outIndexDomain innerDomain
                      , ConstantMatrix outIndexDomain constants
                      ) ]
        setDown _ = throwError "N/A {setDown}"

        setUp ctxt (name, domain@(DomainSet "Explicit" (SetAttrSize size) innerDomain)) =
            case lookup (outName name) ctxt of
                Nothing -> throwError $ vcat $
                    [ "No value for:" <+> pretty (outName name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                Just constant ->
                    case constant of
                        ConstantMatrix _ vals ->
                            return (name, ConstantSet vals)
                        _ -> throwError $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (outName name)
                                , "But got:" <+> pretty constant
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty (DomainSet "Explicit" (SetAttrSize size) innerDomain)
                                ]
        setUp _ _ = throwError "N/A {setUp}"


setExplicitVarSizeWithMarker :: (Applicative m, MonadError Doc m) => Representation m
setExplicitVarSizeWithMarker = Representation chck setDown_ setDown setUp

    where

        chck _ (DomainSet _ (SetAttrSize{}) _) = []
        chck f (DomainSet _ attrs innerDomain) = DomainSet "ExplicitVarSizeWithMarker" attrs <$> f innerDomain
        chck _ _ = []

        nameMarker name = mconcat [name, "_", "ExplicitVarSizeWithMarker", "_Marker"]
        nameMain   name = mconcat [name, "_", "ExplicitVarSizeWithMarker", "_Main"  ]

        getMaxSize attrs innerDomain = case attrs of
            SetAttrMaxSize x -> return x
            SetAttrMinMaxSize _ x -> return x
            _ -> bug $ "domainSize of:" <+> pretty innerDomain

        setDown_ (name, DomainSet _ attrs innerDomain) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 1) maxSize]
            return $ Just
                [ ( nameMarker name
                  , indexDomain
                  )
                , ( nameMain name
                  , DomainMatrix (forgetRepr indexDomain) innerDomain
                  )
                ]
        setDown_ _ = throwError "N/A {setDown_}"

        setDown (name, domain@(DomainSet _ attrs innerDomain), ConstantSet constants) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 1) maxSize]
            maxSizeInt <-
                case maxSize of
                    ConstantInt x -> return x
                    _ -> throwError $ vcat
                            [ "Expecting an integer for the maxSize attribute."
                            , "But got:" <+> pretty maxSize
                            , "When working on:" <+> pretty name
                            , "With domain:" <+> pretty domain
                            ]
            z <- zeroVal innerDomain
            let zeroes = replicate (maxSizeInt - length constants) z
            return $ Just
                [ ( nameMarker name
                  , indexDomain
                  , ConstantInt (length constants)
                  )
                , ( nameMain name
                  , DomainMatrix   (forgetRepr indexDomain) innerDomain
                  , ConstantMatrix (forgetRepr indexDomain) (constants ++ zeroes)
                  )
                ]
        setDown _ = throwError "N/A {setDown}"

        setUp ctxt (name, domain) =
            case (lookup (nameMarker name) ctxt, lookup (nameMain name) ctxt) of
                (Just marker, Just constantMatrix) ->
                    case marker of
                        ConstantInt card ->
                            case constantMatrix of
                                ConstantMatrix _ vals ->
                                    return (name, ConstantSet (take card vals))
                                _ -> throwError $ vcat
                                        [ "Expecting a matrix literal for:" <+> pretty (nameMain name)
                                        , "But got:" <+> pretty constantMatrix
                                        , "When working on:" <+> pretty name
                                        , "With domain:" <+> pretty domain
                                        ]
                        _ -> throwError $ vcat
                                [ "Expecting an integer literal for:" <+> pretty (nameMarker name)
                                , "But got:" <+> pretty marker
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                (Nothing, _) -> throwError $ vcat $
                    [ "No value for:" <+> pretty (nameMarker name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> throwError $ vcat $
                    [ "No value for:" <+> pretty (nameMain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)


setExplicitVarSizeWithFlags :: (Applicative m, MonadError Doc m) => Representation m
setExplicitVarSizeWithFlags = Representation chck setDown_ setDown setUp

    where

        chck _ (DomainSet _ (SetAttrSize{}) _) = []
        chck f (DomainSet _ attrs innerDomain) = DomainSet "ExplicitVarSizeWithFlags" attrs <$> f innerDomain
        chck _ _ = []

        nameFlag name = mconcat [name, "_", "ExplicitVarSizeWithFlags", "_Flags"]
        nameMain name = mconcat [name, "_", "ExplicitVarSizeWithFlags", "_Main"]

        getMaxSize attrs innerDomain = case attrs of
            SetAttrMaxSize x -> return x
            SetAttrMinMaxSize _ x -> return x
            _ -> bug $ "domainSize of:" <+> pretty innerDomain


        setDown_ (name, DomainSet _ attrs innerDomain) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 1) maxSize]
            return $ Just
                [ ( nameFlag name
                  , DomainMatrix (forgetRepr indexDomain) DomainBool
                  )
                , ( nameMain name
                  , DomainMatrix (forgetRepr indexDomain) innerDomain
                  )
                ]
        setDown_ _ = throwError "N/A {setDown_}"

        setDown (name, domain@(DomainSet _ attrs innerDomain), ConstantSet constants) = do
            maxSize <- getMaxSize attrs innerDomain
            let indexDomain = DomainInt [RangeBounded (fromInt 1) maxSize]

            maxSizeInt <-
                case maxSize of
                    ConstantInt x -> return x
                    _ -> throwError $ vcat
                            [ "Expecting an integer for the maxSize attribute."
                            , "But got:" <+> pretty maxSize
                            , "When working on:" <+> pretty name
                            , "With domain:" <+> pretty domain
                            ]
            z <- zeroVal innerDomain
            let zeroes = replicate (maxSizeInt - length constants) z

            let trues  = replicate (length constants)              (ConstantBool True)
            let falses = replicate (maxSizeInt - length constants) (ConstantBool False)

            return $ Just
                [ ( nameFlag name
                  , DomainMatrix   (forgetRepr indexDomain) DomainBool
                  , ConstantMatrix (forgetRepr indexDomain) (trues ++ falses)
                  )
                , ( nameMain name
                  , DomainMatrix   (forgetRepr indexDomain) innerDomain
                  , ConstantMatrix (forgetRepr indexDomain) (constants ++ zeroes)
                  )
                ]
        setDown _ = throwError "N/A {setDown}"

        setUp ctxt (name, domain) =
            case (lookup (nameFlag name) ctxt, lookup (nameMain name) ctxt) of
                (Just flagMatrix, Just constantMatrix) ->
                    case flagMatrix of
                        -- TODO: check if indices match
                        ConstantMatrix _ flags ->
                            case constantMatrix of
                                ConstantMatrix _ vals ->
                                    return (name, ConstantSet [ v
                                                              | (i,v) <- zip flags vals
                                                              , i == ConstantBool True
                                                              ] )
                                _ -> throwError $ vcat
                                        [ "Expecting a matrix literal for:" <+> pretty (nameMain name)
                                        , "But got:" <+> pretty constantMatrix
                                        , "When working on:" <+> pretty name
                                        , "With domain:" <+> pretty domain
                                        ]
                        _ -> throwError $ vcat
                                [ "Expecting a matrix literal for:" <+> pretty (nameFlag name)
                                , "But got:" <+> pretty flagMatrix
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty domain
                                ]
                (Nothing, _) -> throwError $ vcat $
                    [ "No value for:" <+> pretty (nameFlag name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)
                (_, Nothing) -> throwError $ vcat $
                    [ "No value for:" <+> pretty (nameMain name)
                    , "When working on:" <+> pretty name
                    , "With domain:" <+> pretty domain
                    ] ++
                    ("Bindings in context:" : prettyContext ctxt)


