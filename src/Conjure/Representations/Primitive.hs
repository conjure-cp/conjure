{-# LANGUAGE FlexibleContexts #-}

module Conjure.Representations.Primitive
    ( primitive
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Representations.Internal


primitive :: MonadFail m => Representation m
primitive = Representation
    { rCheck = \ _ domain ->
        case domain of
            DomainBool -> [DomainBool]
            DomainInt rs -> [DomainInt rs]
            _ -> []
    , rDownD      = const $ return Nothing
    , rStructural = \ _ _ _ -> return (\ _ _ -> return [] )
    , rDownC      = const $ return Nothing
    , rUp         = \ ctxt (name, _) ->
        case lookup name ctxt of
            Nothing -> fail $ vcat
                $ ("No value for:" <+> pretty name)
                : "Bindings in context:"
                : prettyContext ctxt
            Just c  -> return (name, c)
    }

