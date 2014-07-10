{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Conjure.Representations
    ( down, down_, up
    , down1_, dispatch
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
                    : "In context:"
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
                _ -> down1 primitive p

        dispatchDown_ p@(_, domain, _) =
            case domain of
                DomainTuple{} -> down1_ tuple p
                _ -> down1_ primitive p

        dispatchUp p@(_, domain) ctxt =
            case domain of
                DomainTuple{} -> up1 tuple p ctxt
                _ -> up1 primitive p ctxt

primitive :: MonadError Doc m => ReprActions m
primitive = ReprActions
    { down1  = const $ return Nothing
    , down1_ = const $ return Nothing
    , up1 = \ (name, _) ctxt ->
        case lookup name ctxt of
            Nothing -> throwError $ vcat
                $ ("No value for:" <+> pretty name)
                : "In context:"
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
                    Nothing -> throwError $ vcat
                        [ "No value for:" <+> pretty n
                        , "When working on:" <+> pretty name
                        , "With domain:" <+> pretty (DomainTuple ds)
                        ]
                    Just val -> return val
            -- TODO: check if (length ds == length vals)
            return (name, ConstantTuple vals)
        tupleUp _ _ = throwError "tupleUp def"




