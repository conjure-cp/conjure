{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE FlexibleContexts #-}

module Conjure.Representations
    ( ReprActions(down1, up), down
    , primitive, tuple
    ) where

-- conjure
import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty


data ReprActions m = ReprActions
    { down1 ::       (Text, Domain Representation Constant, Constant)
        -> m (Maybe [(Text, Domain Representation Constant, Constant)])
    , up :: (Text, Domain Representation Constant) -> [(Text, Constant)] -> m (Text, Constant)
    }

down
    :: Monad m
    => ReprActions m
    ->    (Text, Domain Representation Constant, Constant)
    -> m [(Text, Domain Representation Constant, Constant)]
down repr inp = do
    mout <- down1 repr inp
    case mout of
        Nothing -> return [inp]
        Just outs -> liftM concat $ mapM (down repr) outs

primitive :: MonadError Doc m => ReprActions m
primitive = ReprActions
    { down1 = const $ return Nothing
    , up = \ (name, _) ctxt ->
        case lookup name ctxt of
            Nothing -> throwError $ vcat
                $ ("[Conjure.Representations.noOp] name not found:" <+> pretty name)
                : prettyContext ctxt
            Just c  -> return (name, c)
    }

tuple :: MonadError Doc m => ReprActions m
tuple = ReprActions
    { down1 = const $ return Nothing
    , up = \ (name, _) ctxt ->
        case lookup name ctxt of
            Nothing -> throwError $ vcat
                $ ("[Conjure.Representations.noOp] name not found:" <+> pretty name)
                : prettyContext ctxt
            Just c  -> return (name, c)
    }

