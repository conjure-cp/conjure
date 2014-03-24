{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.E.Pipeline.TypeStrengthening ( typeStrengthening ) where

import Language.E
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Bug

import qualified Data.Text as T


typeStrengthening :: MonadConjure m => Spec -> m Spec
typeStrengthening spec = do
    simplified <- simplifySpec spec
    fmap (atMostOneSuchThat False) $ (attributeAcquisition >=> typeChange) simplified


attributeAcquisition :: MonadConjure m => Spec -> m Spec
attributeAcquisition spec@(Spec v statements1) = do

    let findsToConsider = pullFinds spec

    (collectedAttributes, statements2) <- fmap mconcat $ forM (statementAsList statements1) $ \ st -> do
        let cons = pullConstraints st
        if null cons
            then return ([], [st])
            else do
                (attrs,cs) <- fmap mconcat $ forM cons $ \case

                    -- numParts (min&max), partSize (min&max)

                    [eMatch| |parts(&x)| = &n |]                            -> return ([(x, "numParts", Just n)], [])
                    [eMatch| (sum &_ in parts(&x) . 1) = &n |]              -> return ([(x, "numParts", Just n)], [])

                    [eMatch| |parts(&x)| >= &n |]                           -> return ([(x, "minNumParts", Just n)], [])
                    [eMatch| (sum &_ in parts(&x) . 1) >= &n |]             -> return ([(x, "minNumParts", Just n)], [])

                    [eMatch| |parts(&x)| <= &n |]                           -> return ([(x, "maxNumParts", Just n)], [])
                    [eMatch| (sum &_ in parts(&x) . 1) <= &n |]             -> return ([(x, "maxNumParts", Just n)], [])

                    [eMatch| forAll &i in parts(&x) . |&j| = &n |]                  | i == j -> return ([(x, "partSize", Just n)], [])
                    [eMatch| forAll &i in parts(&x) . (sum &_ in &j . 1) = &n |]    | i == j -> return ([(x, "partSize", Just n)], [])

                    [eMatch| forAll &i in parts(&x) . |&j| >= &n |]                 | i == j -> return ([(x, "minPartSize", Just n)], [])
                    [eMatch| forAll &i in parts(&x) . (sum &_ in &j . 1) >= &n |]   | i == j -> return ([(x, "minPartSize", Just n)], [])

                    [eMatch| forAll &i in parts(&x) . |&j| <= &n |]                 | i == j -> return ([(x, "maxPartSize", Just n)], [])
                    [eMatch| forAll &i in parts(&x) . (sum &_ in &j . 1) <= &n |]   | i == j -> return ([(x, "maxPartSize", Just n)], [])


                    -- size, minSize, maxSize

                    [eMatch| |&x| =  &n |]                                  -> return ([(x, "size"   , Just n)],[])
                    [eMatch| (sum &_ in &x . 1) =  &n |]                    -> return ([(x, "size"   , Just n)],[])

                    [eMatch| |&x| >= &n |]                                  -> return ([(x, "minSize", Just n)],[])
                    [eMatch| (sum &_ in &x . 1) >=  &n |]                   -> return ([(x, "minSize", Just n)],[])

                    [eMatch| |&x| <= &n |]                                  -> return ([(x, "maxSize", Just n)],[])
                    [eMatch| (sum &_ in &x . 1) <=  &n |]                   -> return ([(x, "maxSize", Just n)],[])


                    -- minOccur, maxOccur

                    [eMatch| forAll &i : &dom . freq(&x,&j) >= &n |]        | i == j
                                                                            , Just [xMatch| [domX] := domain.mset.inner |] <- x `lookup` findsToConsider
                                                                            , dom == domX
                                                                            -> return ([(x, "minOccur", Just n)],[])

                    [eMatch| forAll &i : &dom . freq(&x,&j) <= &n |]        | i == j
                                                                            , Just [xMatch| [domX] := domain.mset.inner |] <- x `lookup` findsToConsider
                                                                            , dom == domX
                                                                            -> return ([(x, "maxOccur", Just n)],[])


                    -- functional
                    [eMatch| forAll &i : &dom . |&x(&j,_)| = 1 |]           | i == j
                                                                            , Just [xMatch| [domX,_] := domain.relation.inners |] <- x `lookup` findsToConsider
                                                                            , dom == domX
                                                                            -> return ( [ (x, "functional" , Just [eMake| 1 |] )
                                                                                        , (x, "total"      , Nothing           )
                                                                                        ], [])

                    [eMatch| forAll &i : &dom . |&x(&j,_)| <= 1 |]          | i == j
                                                                            , Just [xMatch| [domX,_] := domain.relation.inners |] <- x `lookup` findsToConsider
                                                                            , dom == domX
                                                                            -> return ( [ (x, "functional" , Just [eMake| 1 |] )
                                                                                        ], [])

                    [eMatch| forAll &i : &dom . |&x(_,&j)| = 1 |]           | i == j
                                                                            , Just [xMatch| [domX,_] := domain.relation.inners |] <- x `lookup` findsToConsider
                                                                            , dom == domX
                                                                            -> return ( [ (x, "functional" , Just [eMake| 2 |] )
                                                                                        , (x, "total"      , Nothing           )
                                                                                        ], [])

                    [eMatch| forAll &i : &dom . |&x(_,&j)| <= 1 |]          | i == j
                                                                            , Just [xMatch| [domX,_] := domain.relation.inners |] <- x `lookup` findsToConsider
                                                                            , dom == domX
                                                                            -> return ( [ (x, "functional" , Just [eMake| 2 |] )
                                                                                        ], [])
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

    return $ Spec v $ listAsStatement statements3

typeChange :: MonadConjure m => Spec -> m Spec
typeChange spec@(Spec v statements1) = do

    let findsToConsider = pullFinds spec

    let maxOccur1 = mkAttr ("maxOccur", Just [eMake| 1 |])
    let functional1 = mkAttr ("functional", Just [eMake| 1 |])
    let functional2 = mkAttr ("functional", Just [eMake| 2 |])

    replacements <- fmap catMaybes $ forM findsToConsider $ \ (name, domain) -> case domain of
        [xMatch| [inner] := domain.mset.inner
               | attrs   := domain.mset.attributes.attrCollection
               |]
            | maxOccur1 `elem` attrs
            -> return $ Just (name, ( [xMake| domain.set.inner := [inner]
                                            | domain.set.attributes.attrCollection := (attrs \\ [maxOccur1])
                                            |]
                                    , [eMake| toSet(&name) |]
                                    ))
        [xMatch| [fr,to] := domain.relation.inners
               | attrs   := domain.relation.attributes.attrCollection
               |]
            | functional1 `elem` attrs
            -> return $ Just (name, ( [xMake| domain.function.innerFrom := [fr]
                                            | domain.function.innerTo   := [to]
                                            | domain.function.attributes.attrCollection := (attrs \\ [functional1])
                                            |]
                                    , [eMake| toRelation(&name) |]
                                    ))
        [xMatch| [to,fr] := domain.relation.inners
               | attrs   := domain.relation.attributes.attrCollection
               |]
            | functional2 `elem` attrs
            -> return $ Just (name, ( [xMake| domain.function.innerFrom := [fr]
                                            | domain.function.innerTo   := [to]
                                            | domain.function.attributes.attrCollection := (attrs \\ [functional2])
                                            |]
                                    , [eMake| toRelation(&name) |]
                                    ))
        _ -> return Nothing

    let decorate x | Just (_,y) <- x `lookup` replacements = y
        decorate (Tagged t xs) = Tagged t (map decorate xs)
        decorate x = x

    statements2 <- forM (statementAsList statements1) $ \case
        [xMatch| [name] := topLevel.declaration.find.name
               |] | Just (domain,_) <- name `lookup` replacements
            -> return [xMake| topLevel.declaration.find.name := [name]
                            | topLevel.declaration.find.domain := [domain]
                            |]
        s -> return (decorate s)

    return $ Spec v $ listAsStatement statements2

-- pullThoseWithDomain :: MonadConjure m => Spec -> T.Text -> m [(E,E)]
-- pullThoseWithDomain (Spec _ statements) domainStr =
--     case lexAndParse (inCompleteFile parseDomain) domainStr of
--         Left  parseError -> bug $ vcat [ "pullThoseWithDomain, parse error", pretty parseError ]
--         Right domain -> do
--             mkLog "typeStrengthening ~~ pullThoseWithDomain domain" $ pretty domain
--             fmap concat $ forM (statementAsList statements) $ \case
--                 [xMatch| [name] := topLevel.declaration.find.name
--                        | [dom]  := topLevel.declaration.find.domain |] -> do
--                    (matches, _) <- patternMatch domain dom
--                    if matches
--                        then return [(name, dom)]
--                        else return []
--                 _ -> return []

pullConstraints :: E -> [E]
pullConstraints [xMatch| xs := topLevel.suchThat |] = xs
pullConstraints _ = []

pullFinds :: Spec -> [(E,E)]
pullFinds (Spec _ x) = mapMaybe pullFind (statementAsList x)
    where pullFind [xMatch| [name] := topLevel.declaration.find.name
                          | [dom]  := topLevel.declaration.find.domain |] = Just (name,dom)
          pullFind _ = Nothing

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

updateAttributes newAttrs
    [xMatch| attrs := domain.function.attributes.attrCollection
           | [fr]  := domain.function.innerFrom
           | [to]  := domain.function.innerTo
           |] = return [xMake| domain.function.attributes.attrCollection := attrs'
                             | domain.function.innerFrom := [fr]
                             | domain.function.innerTo := [to]
                             |]
        where attrs' = newAttrs ++ attrs

updateAttributes newAttrs
    [xMatch| inners  := domain.relation.inners
           | attrs   := domain.relation.attributes.attrCollection
           |] = return [xMake| domain.relation.inners := inners
                             | domain.relation.attributes.attrCollection := attrs'
                             |]
        where attrs' = newAttrs ++ attrs

updateAttributes newAttrs
    [xMatch| [inner] := domain.partition.inner
           | attrs   := domain.partition.attributes.attrCollection
           |] = return [xMake| domain.partition.inner := [inner]
                             | domain.partition.attributes.attrCollection := attrs'
                             |]
        where attrs' = newAttrs ++ attrs

updateAttributes _ dom = bug $ vcat [ "don't know how to update this domain"
                                    , pretty dom
                                    ]

mkAttr :: (T.Text, Maybe E) -> E
mkAttr (n, Nothing) = [xMake| attribute.name.reference := [Prim (S n)] |]
mkAttr (n, Just v ) = [xMake| attribute.nameValue.name.reference := [Prim (S n)]
                            | attribute.nameValue.value          := [v]
                            |]








