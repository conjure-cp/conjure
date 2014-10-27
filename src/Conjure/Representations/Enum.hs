{-# LANGUAGE FlexibleContexts #-}

module Conjure.Representations.Enum
    ( enum
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.DomainSize
import Conjure.Language.Pretty
import Conjure.Representations.Internal


enum :: MonadFail m => Representation m
enum = Representation check downD structural downC up
    where
        check _ (DomainEnum name rs) = [DomainEnum name rs]
        check _ _ = []

        varOut  name ename = mconcat [name, "_FromEnum_", ename]
        sizeOut name       = mconcat [name, "_EnumSize" ]

        downD (name, DomainEnum ename Nothing) = return $ Just
            [ ( varOut name ename
              , DomainInt [RangeBounded (fromInt 1) (fromName (sizeOut ename))]
              )
            ]
        downD (name, DomainEnum ename (Just (vals, ranges))) = do
            let ranges' =
                    if null ranges
                        then [RangeBounded (fromInt 1) (fromInt (length vals))]
                        else fmap (fmap (fromInt . enumNameToInt vals)) ranges
            return $ Just
                [ ( varOut name ename
                  , DomainInt ranges'
                  )
                ]
        downD _ = fail "N/A {enum.downD}"

        structural = const $ return Nothing

        downC (name, DomainEnum ename (Just (vals, ranges)), ConstantEnum _ _ val) = do
            let ranges' =
                    if null ranges
                        then [RangeBounded (fromInt 1) (fromInt (length vals))]
                        else fmap (fmap (fromInt . enumNameToInt vals)) ranges
            return $ Just
                [ ( varOut name ename
                  , DomainInt ranges'
                  , ConstantInt (enumNameToInt vals val)
                  )
                ]
        downC _ = fail "N/A {enum.downC}"

        up ctxt (name, domain) =
            case domain of
                DomainEnum ename (Just (vals,_)) -> 
                    case lookup (varOut name ename) ctxt of
                        Nothing -> fail $ vcat
                            $ ("No value for:" <+> pretty name)
                            : "Bindings in context:"
                            : prettyContext ctxt
                        Just (ConstantInt c) -> return (name, ConstantEnum ename vals (at vals (c-1)))
                        Just c -> fail ("Expecting an integer, but got" <+> pretty c)
                d -> fail ("N/A {enum.up}" <++> pretty (show d))

