{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns #-}

module Language.E.Pipeline.ApplyRepr where

import Language.E
import Language.E.Pipeline.RuleReprToFunction ( ruleReprToFunction )

applyRepr :: (Functor m, Monad m) => Spec -> [RuleRepr] -> CompE m Spec
applyRepr spec rules = let mfunc = ruleReprToFunction rules in case mfunc of
    Left es    -> err ErrFatal $ prettyErrors "There were errors." es
    Right func -> do

        let Spec _ statements = spec

        let topLevels' = [ (x,d) | x@[xMatch| [d] := topLevel.declaration.find .domain |] <- statements ]
                      ++ [ (x,d) | x@[xMatch| [d] := topLevel.declaration.given.domain |] <- statements ]
        let topLevels = [ (x,d) | (x,d) <- topLevels', domainNeedsRepresentation d ]

        candidates <- forM topLevels $ \ (x,d) -> do
            ys <- func d
            case ys of
                [] -> err ErrFatal $ "No representation rule matches domain:" <+> pretty x
                _  -> do
                    mkLog "representation" $ pretty (length ys) <+> "different representation option(s) for: " <+> pretty x
                    forM_ ys $ \ (ruleName, reprName, newDom, cons) -> do
                        mkLog "newDom" $ pretty newDom
                        forM_ cons $ \ con -> mkLog "cons" $ pretty con
                    return ys

        forM_ topLevels $ \ i -> mkLog "applyRepr" $ prettyAsPaths $ fst i
        ys <- func $ snd $ head topLevels
        case ys of
            [] -> return spec
            _  -> return spec



domainNeedsRepresentation :: E -> Bool
domainNeedsRepresentation [xMatch| _ := domain.set       |] = True
domainNeedsRepresentation [xMatch| _ := domain.mset      |] = True
domainNeedsRepresentation [xMatch| _ := domain.function  |] = True
domainNeedsRepresentation [xMatch| _ := domain.relation  |] = True
domainNeedsRepresentation [xMatch| _ := domain.partition |] = True
domainNeedsRepresentation _ = False
