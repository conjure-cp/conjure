{-# LANGUAGE QuasiQuotes #-}

module Conjure.Representations.Primitive
    ( primitive
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language
import Conjure.Representations.Internal


primitive :: forall m . MonadFail m => Representation m
primitive = Representation
    { rCheck = \ _ domain -> return $
        case domain of
            DomainBool -> [DomainBool]
            DomainIntE x -> [DomainIntE x]
            DomainInt t rs -> [DomainInt t rs]
            _ -> []
    , rDownD      = const $ return Nothing
    , rStructural = \ _ _ _ -> return (\ _ -> return [] )
    , rDownC      = const $ return Nothing
    , rUp         = \ ctxt (name, _) ->
        case lookup name ctxt of
            Nothing -> fail $ vcat
                $ ("No value for:" <+> pretty name)
                : "Bindings in context:"
                : prettyContext ctxt
            Just c  -> return (name, c)
    , rSymmetryOrdering = \ _innerSO _downX1 inp _name domain -> return $
        case domain of
            DomainBool -> [essence| [toInt(&inp)] |]
            _          -> [essence| [&inp] |]
    }

