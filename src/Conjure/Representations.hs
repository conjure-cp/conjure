{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conjure.Representations
    ( down, down_, up
    , down1, down1_, up1, dispatch
    ) where

-- conjure
import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty

-- text
import Data.Text ( pack )


data ReprActions m = ReprActions
    { down1  ::      (Text, Domain Representation Constant)
        -> m (Maybe [(Text, Domain Representation Constant)])
    , down1_ ::      (Text, Domain Representation Constant, Constant)
        -> m (Maybe [(Text, Domain Representation Constant, Constant)])
    , up1    :: (Text, Domain Representation Constant)
             -> [(Text, Constant)] -> m (Text, Constant)
    }


down
    :: MonadError Doc m
    =>    (Text, Domain Representation Constant)
    -> m [(Text, Domain Representation Constant)]
down inp = do
    mout <- down1 dispatch inp
    case mout of
        Nothing -> return [inp]
        Just outs -> liftM concat $ mapM down outs

down_
    :: MonadError Doc m
    =>    (Text, Domain Representation Constant, Constant)
    -> m [(Text, Domain Representation Constant, Constant)]
down_ inp = do
    mout <- down1_ dispatch inp
    case mout of
        Nothing -> return [inp]
        Just outs -> liftM concat $ mapM down_ outs

up
    :: MonadError Doc m
    => (Text, Domain Representation Constant)
    ->  [(Text, Constant)]
    -> m (Text, Constant)
up (name, highDomain) ctxt = do
    toDescend'
        :: Maybe [(Text, Domain Representation Constant)]
        <- down1 dispatch (name, highDomain)
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
                 :: [(Text, Constant)]
                 <- sequence [ up (n,d) ctxt | (n,d) <- toDescend ]
            up1 dispatch (name, highDomain) midConstants

dispatch :: MonadError Doc m => ReprActions m
dispatch = ReprActions
    { down1 = dispatchDown
    , down1_ = dispatchDown_
    , up1 = dispatchUp
    }

    where

        dispatchDown p@(_, domain) =
            case domain of
                DomainTuple{} -> down1 tuple p
                DomainMatrix{} -> down1 matrix p
                _ -> down1 primitive p

        dispatchDown_ p@(_, domain, _) =
            case domain of
                DomainTuple{} -> down1_ tuple p
                DomainMatrix{} -> down1_ matrix p
                _ -> down1_ primitive p

        dispatchUp p@(_, domain) ctxt =
            case domain of
                DomainTuple{} -> up1 tuple p ctxt
                DomainMatrix{} -> up1 matrix p ctxt
                _ -> up1 primitive p ctxt

primitive :: MonadError Doc m => ReprActions m
primitive = ReprActions
    { down1  = const $ return Nothing
    , down1_ = const $ return Nothing
    , up1 = \ (name, _) ctxt ->
        case lookup name ctxt of
            Nothing -> throwError $ vcat
                $ ("No value for:" <+> pretty name)
                : "Bindings in context:"
                : prettyContext ctxt
            Just c  -> return (name, c)
    }

tuple :: MonadError Doc m => ReprActions m
tuple = ReprActions
    { down1 = tupleDown
    , down1_ = tupleDown_
    , up1 = tupleUp
    }

    where

        mkName name i = mconcat [name, "_", pack (show (i :: Int))]

        tupleDown (name, DomainTuple ds) = return $ Just
            [ (mkName name i, d)
            | i <- [1..]
            | d <- ds
            ]
        tupleDown _ = return Nothing

        -- TODO: check if (length ds == length cs)
        tupleDown_ (name, DomainTuple ds, ConstantTuple cs) = return $ Just
            [ (mkName name i, d, c)
            | i <- [1..]
            | d <- ds
            | c <- cs
            ]
        tupleDown_ _ = return Nothing

        tupleUp (name, DomainTuple ds) ctxt = do
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
        tupleUp _ _ = throwError "tupleUp def"

matrix :: MonadError Doc m => ReprActions m
matrix = ReprActions
    { down1 = matrixDown
    , down1_ = matrixDown_
    , up1 = matrixUp
    }

    where

        matrixDown (name, DomainMatrix index inner) = do
            mres <- down1 dispatch (name, inner)
            case mres of
                Nothing -> return Nothing
                Just mids -> return $ Just [ (n, DomainMatrix index d) | (n, d) <- mids ]
        matrixDown _ = return Nothing

        -- TODO: check if indices are the same
        matrixDown_ (name, DomainMatrix index inner, ConstantMatrix _index2 constants) = do
            mids1
                :: [Maybe [(Text, Domain Representation Constant, Constant)]]
                <- sequence [ down1_ dispatch (name, inner, c) | c <- constants ]
            let mids2 = catMaybes mids1
            if null mids2                                       -- if all were `Nothing`s
                then return Nothing
                else
                    if length mids2 == length mids1             -- if all were `Just`s
                        then do
                            let
                                mids3 :: [(Text, Domain Representation Constant, [Constant])]
                                mids3 = [ ( head [ n | (n,_,_) <- line ]
                                          , head [ d | (_,d,_) <- line ]
                                          ,      [ c | (_,_,c) <- line ]
                                          )
                                        | line <- transpose mids2
                                        ]
                            return $ Just
                                [ ( n
                                  , DomainMatrix index d
                                  , ConstantMatrix index cs
                                  )
                                | (n, d, cs) <- mids3
                                ]
                        else
                            throwError $ vcat
                                [ "This is weird. Heterogeneous matrix literal?"
                                , "When working on:" <+> pretty name
                                , "With domain:" <+> pretty (DomainMatrix index inner)
                                ]
        matrixDown_ _ = return Nothing

        matrixUp (name, DomainMatrix index inner) ctxt = do

            mid1
                :: Maybe [(Text, Domain Representation Constant)]
                <- down1 dispatch (name, inner)

            case mid1 of
                Nothing ->
                    -- the inner domain doesn't require refinement
                    -- there needs to be a binding with "name"
                    -- and we just pass it through
                    case lookup name ctxt of
                        Nothing -> throwError $ vcat $
                            [ "No value for:" <+> pretty name
                            , "With domain:" <+> pretty (DomainMatrix index inner)
                            ] ++
                            ("Bindings in context:" : prettyContext ctxt)
                        Just constant -> return (name, constant)
                Just mid2 -> do
                    -- the inner domain needs refinement
                    -- there needs to be bindings for each name in (map fst mid2)
                    -- we find those bindings, call (up1 dispatch name inner) on them, then lift
                    mid3
                        :: [(Text, [Constant])]
                        <- forM mid2 $ \ (n, _) -> do
                            case lookup n ctxt of
                                Nothing -> throwError $ vcat $
                                    [ "No value for:" <+> pretty n
                                    , "When working on:" <+> pretty name
                                    , "With domain:" <+> pretty (DomainMatrix index inner)
                                    ] ++
                                    ("Bindings in context:" : prettyContext ctxt)
                                Just constant ->
                                    -- this constant is a ConstantMatrix, containing one component of the things to go into up1
                                    case constant of
                                        ConstantMatrix _ c -> return (n,c)
                                        _ -> throwError $ vcat
                                            [ "Expecting a matrix literal for:" <+> pretty n
                                            , "But got:" <+> pretty constant
                                            , "When working on:" <+> pretty name
                                            , "With domain:" <+> pretty (DomainMatrix index inner)
                                            ]

                    let midNames     = map fst mid3
                    let midConstants = map snd mid3

                    mid4
                        :: [(Text, Constant)]
                        <- sequence
                            [ up1 dispatch (name, inner) (zip midNames cs)
                            | cs <- transpose midConstants
                            ]
                    let values = map snd mid4
                    return (name, ConstantMatrix index values)

        matrixUp _ _ = throwError "matrixUp def"






