{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.E.Pipeline.TypeStrengthening ( typeStrengthening ) where

import Language.E
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Bug

import qualified Data.Text as T
-- import Text.Groom ( groom )


typeStrengthening :: MonadConjure m => Spec -> m Spec
typeStrengthening spec = fmap (atMostOneSuchThat False) $ setCardinality spec


setCardinality :: MonadConjure m => Spec -> m Spec
setCardinality spec@(Spec v statements1) = do

    setsToConsider  <- fmap (map fst) $ pullThoseWithDomain spec "set (..) of &tau"
    msetsToConsider <- fmap (map fst) $ pullThoseWithDomain spec "mset (..) of &tau"

    let findsToConsider = setsToConsider ++ msetsToConsider

    (collectedAttributes, statements2) <- fmap mconcat $ forM (statementAsList statements1) $ \ st -> do
        let cons = pullConstraints st
        if null cons
            then return ([], [st])
            else do
                (attrs,cs) <- fmap mconcat $ forM cons $ \case
                    [eMatch| |&x| =  &n |]                          | x `elem` findsToConsider -> return ([(x,"size"   ,n)],[])
                    [eMatch| (sum &_ in &x . 1) =  &n |]            | x `elem` findsToConsider -> return ([(x,"size"   ,n)],[])

                    [eMatch| |&x| >= &n |]                          | x `elem` findsToConsider -> return ([(x,"minSize",n)],[])
                    [eMatch| (sum &_ in &x . 1) >=  &n |]           | x `elem` findsToConsider -> return ([(x,"minSize",n)],[])

                    [eMatch| |&x| <= &n |]                          | x `elem` findsToConsider -> return ([(x,"maxSize",n)],[])
                    [eMatch| (sum &_ in &x . 1) <=  &n |]           | x `elem` findsToConsider -> return ([(x,"maxSize",n)],[])

                    c -> return ([],[c])
                return (attrs, [ [xMake| topLevel.suchThat := cs |] ])

    statements3 <- forM statements2 $ \ s  -> case s of
        [xMatch| [name  ] := topLevel.declaration.find.name
               | [domain] := topLevel.declaration.find.domain
               |] -> do
            let collectedAttributesForThis = [ (attr, val) | (name', attr, val) <- collectedAttributes, name == name' ]
            if null collectedAttributesForThis
                then return s
                else do
                    domain' <- updateAttributes (map mkAttr collectedAttributesForThis) domain
                    return [xMake| topLevel.declaration.find.name   := [name]
                                 | topLevel.declaration.find.domain := [domain']
                                 |]
        _ -> return s

    -- error $ groom setsToConsider
    -- error $ groom constraints
    -- error $ groom sizes

    return $ Spec v $ listAsStatement statements3

pullThoseWithDomain :: MonadConjure m => Spec -> T.Text -> m [(E,E)]
pullThoseWithDomain (Spec _ statements) domainStr =
    case lexAndParse (inCompleteFile parseDomain) domainStr of
        Left  parseError -> bug $ vcat [ "pullThoseWithDomain, parse error", pretty parseError ]
        Right domain -> do
            mkLog "typeStrengthening ~~ pullThoseWithDomain domain" $ pretty domain
            fmap concat $ forM (statementAsList statements) $ \case
                [xMatch| [name] := topLevel.declaration.find.name
                       | [dom]  := topLevel.declaration.find.domain |] -> do
                   (matches, _) <- patternMatch domain dom
                   if matches
                       then return [(name, dom)]
                       else return []
                _ -> return []

pullConstraints :: E -> [E]
pullConstraints [xMatch| xs := topLevel.suchThat |] = xs
pullConstraints _ = []

updateAttributes
    :: MonadConjure m
    => [E]                  -- attributes
    -> E                    -- domain
    -> m E                  -- modified domain

updateAttributes newAttrs
    [xMatch| [inner] := domain.set.inner
           | attrs   := domain.set.attributes.attrCollection
           |] = return [xMake| domain.set.inner := [inner]
                             | domain.set.attributes.attrCollection := attrs'
                             |]
        where attrs' = newAttrs ++ attrs

updateAttributes newAttrs
    [xMatch| [inner] := domain.mset.inner
           | attrs   := domain.mset.attributes.attrCollection
           |] = return [xMake| domain.mset.inner := [inner]
                             | domain.mset.attributes.attrCollection := attrs'
                             |]
        where attrs' = newAttrs ++ attrs

updateAttributes _ dom = bug $ vcat [ "don't know how to update this domain"
                                    , pretty dom
                                    ]

mkAttr :: (T.Text, E) -> E
mkAttr (n,v) = [xMake| attribute.nameValue.name.reference := [Prim (S n)]
                     | attribute.nameValue.value          := [v]
                     |]









