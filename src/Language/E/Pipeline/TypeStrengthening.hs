{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.E.Pipeline.TypeStrengthening ( typeStrengthening ) where

import Language.E
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Bug
-- import Utils.DebugPretty

import qualified Data.Text as T



typeStrengthening :: MonadConjure m => Spec -> m Spec
typeStrengthening = return
    >=> simplifySpec
    >=> attributeAcquisition
    >=> typeChange
    >=> simplifySpec
    >=> return . atMostOneSuchThat False


attributeAcquisition :: MonadConjure m => Spec -> m Spec
attributeAcquisition spec@(Spec v statements0) = do

    statements1 <- preprocess statements0

    let findsToConsider = pullFinds spec

    (collectedAttributes, statements2) <- fmap mconcat $ forM (statementAsList statements1) $ \ st -> do
        introduceStuff st
        let cons = pullConstraints st
        if null cons
            then return ([], [st])
            else do
                (attrs,cs) <- fmap mconcat $ mapM (nestedMatch findsToConsider) cons
                return (attrs, [ [xMake| topLevel.suchThat := cs |] ])

    -- mkLog "attributeAcquisition" $ debugPretty collectedAttributes

    statements3 <- forM statements2 $ \ s  -> case s of
        [xMatch| [name  ] := topLevel.declaration.find.name
               | [domain] := topLevel.declaration.find.domain
               |] -> do
            let collectedAttributesForThis = [ (lvl, attr, val) | (name', lvl, attr, val) <- collectedAttributes, name == name' ]
            if null collectedAttributesForThis
                then return s
                else do
                    let fs = foldr1 (>=>) [ updateAttributes lvl [mkAttr (attr,val)] | (lvl, attr, val) <- collectedAttributesForThis ]
                    domain' <- fs domain
                    return [xMake| topLevel.declaration.find.name   := [name]
                                 | topLevel.declaration.find.domain := [domain']
                                 |]
        _ -> return s

    return $ Spec v $ listAsStatement statements3

preprocess :: MonadConjure m => E -> m E
preprocess = sumAsCardinality

sumAsCardinality :: MonadConjure m => E -> m E
sumAsCardinality [eMatch| sum &_ in &x . 1 |] = return [eMake| |&x| |]
sumAsCardinality (Tagged t xs) = Tagged t <$> mapM sumAsCardinality xs
sumAsCardinality p = return p


nestedMatch
    :: MonadConjure m => [(E,E)] -> E
    -> m ( [ ( E            -- the decision variable
             , Int          -- levels of nesting
             , Text         -- the attribtue
             , Maybe E      -- value of the attribute, Nothing if flag
             ) ]
         , [E]              -- expressions to keep. [] if constraint isn't needed any more.
         )                  -- in general, if the first component is [], we've learned nothing from this constraint.
nestedMatch findsToConsider cons = do
    out1 <- directMatch findsToConsider cons
    case out1 of
        ([], _) -> do
            case cons of
                [eMatch| forAll &i in &x . &body |] -> do
                    introduceStuff cons
                    domX <- domainOf x
                    let findsToConsider' = case innerDomainOf domX of
                                                Just t -> (i, t) : findsToConsider
                                                Nothing -> bug $ vcat [ "nestedMatch-1", pretty x, pretty domX ]
                    out2 <- nestedMatch findsToConsider' body
                    case out2 of
                        ([], _) -> return ([], [cons])
                        ([(decvar, lvl, attr, val)], keeps) | i == decvar ->
                            let
                                liftKeep k  = [eMake| forAll &i in &x . &k |]
                                keepsLifted = map liftKeep keeps
                            in  return ([(x, 1+lvl, attr, val)], keepsLifted)
                        _ -> bug $ vcat [ "nestedMatch-2", pretty cons ]
                _ -> return ([], [cons])
        (attrs, keep) -> do
            return ([(decvar, 0, attr, val) | (decvar, attr, val) <- attrs ], keep)

directMatch
    :: MonadConjure m => [(E,E)] -> E
    -> m ( [ ( E            -- the decision variable
             , Text         -- the attribtue
             , Maybe E      -- value of the attribute, Nothing if flag
             ) ]
         , [E]              -- expressions to keep. [] if constraint isn't needed any more.
         )                  -- in general, if the first component is [], we've learned nothing from this constraint.
directMatch findsToConsider cons = case cons of

    -- numParts (min&max), partSize (min&max)

    c@[eMatch| |parts(&x)| =  &n |]                                   -> returnIfCat n c ([(x, "numParts"   , Just n)], [])
    c@[eMatch| |parts(&x)| >= &n |]                                   -> returnIfCat n c ([(x, "minNumParts", Just n)], [])
    c@[eMatch| |parts(&x)| <= &n |]                                   -> returnIfCat n c ([(x, "maxNumParts", Just n)], [])

    c@[eMatch| forAll &i in parts(&x) . |&j| =  &n |]     | i == j    -> returnIfCat n c ([(x, "partSize"   , Just n)], [])
    c@[eMatch| forAll &i in parts(&x) . |&j| >= &n |]     | i == j    -> returnIfCat n c ([(x, "minPartSize", Just n)], [])
    c@[eMatch| forAll &i in parts(&x) . |&j| <= &n |]     | i == j    -> returnIfCat n c ([(x, "maxPartSize", Just n)], [])

    -- regular & complete for partitions

    [eMatch| forAll &i in parts(&x) . forAll &j in parts(&x2) . |&i2| = |&j2| |]
        | i == i2, j == j2, x == x2
        -> return ([(x, "regular", Nothing)], [])

    [eMatch| forAll &i : &dom . &j in &x |]
        | i == j
        , Just [xMatch| [domX] := domain.partition.inner |] <- x `lookup` findsToConsider
        , dom == domX
        -> return ([(x, "complete", Nothing)], [])

    -- total function, because every value is assigned

    c@[eMatch| forAll &i : &dom . &x(&j) = &_ |]
        | i == j
        , Just [xMatch| [domX] := domain.function.innerFrom |] <- x `lookup` findsToConsider
        , dom == domX
        -> return ([(x, "total", Nothing)], [c])

    -- size, minSize, maxSize

    c@[eMatch| |&x| =  &n |] -> returnIfCat n c ([(x, "size"   , Just n)], [])
    c@[eMatch| |&x| >= &n |] -> returnIfCat n c ([(x, "minSize", Just n)], [])
    c@[eMatch| |&x| <= &n |] -> returnIfCat n c ([(x, "maxSize", Just n)], [])


    -- minOccur, maxOccur

    c@[eMatch| forAll &i : &dom . freq(&x,&j) >= &n |]      | i == j
                                                            , Just [xMatch| [domX] := domain.mset.inner |] <- x `lookup` findsToConsider
                                                            , dom == domX
                                                            -> returnIfCat n c ([(x, "minOccur", Just n)],[])

    c@[eMatch| forAll &i : &dom . freq(&x,&j) <= &n |]      | i == j
                                                            , Just [xMatch| [domX] := domain.mset.inner |] <- x `lookup` findsToConsider
                                                            , dom == domX
                                                            -> returnIfCat n c ([(x, "maxOccur", Just n)],[])

    -- functional, because assigned
    c@[eMatch| forAll &i : &dom . &x(&j,_) = {&_} |]        | i == j
                                                            , Just [xMatch| [domX,_] := domain.relation.inners |] <- x `lookup` findsToConsider
                                                            , dom == domX
                                                            -> return ( [ (x, "functional" , Just [eMake| tuple(1) |] )
                                                                        , (x, "total"      , Nothing           )
                                                                        ], [c])
    c@[eMatch| forAll &i : &dom . &x(_,&j) = {&_} |]        | i == j
                                                            , Just [xMatch| [_,domX] := domain.relation.inners |] <- x `lookup` findsToConsider
                                                            , dom == domX
                                                            -> return ( [ (x, "functional" , Just [eMake| tuple(2) |] )
                                                                        , (x, "total"      , Nothing           )
                                                                        ], [c])

    c -> relationFunctional findsToConsider c


relationFunctional
    :: MonadConjure m => [(E,E)] -> E
    -> m ( [ ( E            -- the decision variable
             , Text         -- the attribtue
             , Maybe E      -- value of the attribute, Nothing if flag
             ) ]
         , [E]              -- expressions to keep. [] if constraint isn't needed any more.
         )                  -- in general, if the first component is [], we've learned nothing from this constraint.
relationFunctional findsToConsider cons = go [] cons
    where
        go quans [eMatch| forAll &i : &dom . &body |] = go ((i,dom):quans) body
        go quans [xMatch| [Prim (S "=")] := binOp.operator
                        | [r]   := binOp.left.operator.twoBars.functionApply.actual
                        | args  := binOp.left.operator.twoBars.functionApply.args
                        | [Prim (I 1)] := binOp.right.value.literal
                        |]
            | Just [xMatch| domR := domain.relation.inners |] <- r `lookup` findsToConsider
            = do
                -- domR and {dom of args} need to be pair-wise equal
                -- OR underscore.
                -- output is those indices were it is equal.
                let quantifiers = [ i
                                  | (i,a,d) <- zip3 [ 1::Integer .. ] args domR
                                  , a /= [eMake| _ |]
                                  , let aDom = lookup a quans
                                  , Just d == aDom
                                  ]
                let eInt i = [xMake| value.literal := [Prim (I i)] |]
                return ( [ (r, "functional" , Just [xMake| value.tuple.values := (map eInt quantifiers) |] )
                         , (r, "total"      , Nothing )
                         ], [] )                                                                        
        go quans [xMatch| [Prim (S "<=")] := binOp.operator
                        | [r]   := binOp.left.operator.twoBars.functionApply.actual
                        | args  := binOp.left.operator.twoBars.functionApply.args
                        | [Prim (I 1)] := binOp.right.value.literal
                        |]
            | Just [xMatch| domR := domain.relation.inners |] <- r `lookup` findsToConsider
            = do
                -- same as above case, except not `total`
                let quantifiers = [ i
                                  | (i,a,d) <- zip3 [ 1::Integer .. ] args domR
                                  , a /= [eMake| _ |]
                                  , let aDom = lookup a quans
                                  , Just d == aDom
                                  ]
                let eInt i = [xMake| value.literal := [Prim (I i)] |]
                return ( [ (r, "functional" , Just [xMake| value.tuple.values := (map eInt quantifiers) |] )
                         ], [] )                                                                        
        go _ _ = return ([],[cons]) -- error $ show $ prettyAsPaths con

returnIfCat
    :: MonadConjure m
    => E                                -- the attribute value
    -> E                                -- the original constraint
    ->   ([(E, Text, Maybe E)], [E])    -- result of TS
    -> m ([(E, Text, Maybe E)], [E])    -- result of TS-modulo-category-checking
returnIfCat n cons ret = do
    nCat <- categoryOf n
    if nCat <= CatParameter
        then return ret
        else return ([],[cons])

typeChange :: MonadConjure m => Spec -> m Spec
typeChange spec@(Spec v statements1) = do

    let findsToConsider = pullFinds spec
    let maxOccur1 = mkAttr ("maxOccur", Just [eMake| 1 |])

    replacements <- fmap catMaybes $ forM findsToConsider $ \ (name, domain) -> case domain of
        [xMatch| [inner] := domain.mset.inner
               | attrs   := domain.mset.attributes.attrCollection
               |]
            | maxOccur1 `elem` attrs
            -> return $ Just (name, ( [xMake| domain.set.inner := [inner]
                                            | domain.set.attributes.attrCollection := (attrs \\ [maxOccur1])
                                            |]
                                    , [eMake| toMSet(&name) |]
                                    ))
        [xMatch| inners := domain.relation.inners
               | attrs  := domain.relation.attributes.attrCollection
               |]
            | functionalAttr'                                    <- "functional" `lookupAttr` attrs
            , let functionalAttr = mkAttr ("functional", functionalAttr')
            , Just [xMatch| functionals := value.tuple.values |] <- "functional" `lookupAttr` attrs
            -> do
                let
                    eInt i = [xMake| value.literal := [Prim (I i)] |]
                    eIntOut [xMatch| [Prim (I i)] := value.literal |] = i
                    eIntOut _ = bug "eIntOut"
                let functionals' = map eIntOut functionals
                let nonfunctionals' = [1 .. genericLength inners] \\ functionals'
                let nonfunctionals = map eInt nonfunctionals'
                let frs = map (\ i -> genericIndex inners (i-1)) functionals'
                let tos = inners \\ frs
                let fr = case frs of
                            [i] -> i
                            _   -> [xMake| domain.tuple.inners := frs |]
                let to = case tos of
                            [i] -> i
                            _   -> [xMake| domain.tuple.inners := tos |]
                let permuteTuple = [xMake| value.tuple.values := functionals ++ nonfunctionals |]

                -- wrap in a permute only if it is needed.
                let decorator = if functionals' ++ nonfunctionals' == [1 .. genericLength inners]
                                    then [eMake| toRelation(&name) |]
                                    else [eMake| permute(toRelation(&name),&permuteTuple) |]

                return $ Just (name, ( [xMake| domain.function.innerFrom := [fr]
                                             | domain.function.innerTo   := [to]
                                             | domain.function.attributes.attrCollection := (attrs \\ [functionalAttr])
                                             |]
                                     , decorator
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
    => Int                  -- nesting level, 0 for topmost
    -> [E]                  -- attributes
    -> E                    -- domain
    -> m E                  -- modified domain

updateAttributes 0 newAttrs
    [xMatch| [inner] := domain.set.inner
           | attrs   := domain.set.attributes.attrCollection
           |] = return [xMake| domain.set.inner := [inner]
                             | domain.set.attributes.attrCollection := attrs'
                             |]
        where attrs' = newAttrs ++ attrs

updateAttributes 0 newAttrs
    [xMatch| [inner] := domain.mset.inner
           | attrs   := domain.mset.attributes.attrCollection
           |] = return [xMake| domain.mset.inner := [inner]
                             | domain.mset.attributes.attrCollection := attrs'
                             |]
        where attrs' = newAttrs ++ attrs

updateAttributes 0 newAttrs
    [xMatch| attrs := domain.function.attributes.attrCollection
           | [fr]  := domain.function.innerFrom
           | [to]  := domain.function.innerTo
           |] = return [xMake| domain.function.attributes.attrCollection := attrs'
                             | domain.function.innerFrom := [fr]
                             | domain.function.innerTo := [to]
                             |]
        where attrs' = newAttrs ++ attrs

updateAttributes 0 newAttrs
    [xMatch| inners  := domain.relation.inners
           | attrs   := domain.relation.attributes.attrCollection
           |] = return [xMake| domain.relation.inners := inners
                             | domain.relation.attributes.attrCollection := attrs'
                             |]
        where attrs' = newAttrs ++ attrs

updateAttributes 0 newAttrs
    [xMatch| [inner] := domain.partition.inner
           | attrs   := domain.partition.attributes.attrCollection
           |] = return [xMake| domain.partition.inner := [inner]
                             | domain.partition.attributes.attrCollection := attrs'
                             |]
        where attrs' = newAttrs ++ attrs


updateAttributes lvl newAttrs
    [xMatch| [index] := domain.matrix.index
           | [inner] := domain.matrix.inner
           |] = do
               inner' <- updateAttributes (lvl-1) newAttrs inner
               return [xMake| domain.matrix.index := [index]
                            | domain.matrix.inner := [inner']
                            |]

updateAttributes lvl newAttrs
    [xMatch| [inner] := domain.set.inner
           | attrs   := domain.set.attributes.attrCollection
           |] = do
               inner' <- updateAttributes (lvl-1) newAttrs inner
               return [xMake| domain.set.inner := [inner']
                            | domain.set.attributes.attrCollection := attrs
                            |]

updateAttributes lvl newAttrs
    [xMatch| [inner] := domain.mset.inner
           | attrs   := domain.mset.attributes.attrCollection
           |] = do
               inner' <- updateAttributes (lvl-1) newAttrs inner
               return [xMake| domain.mset.inner := [inner']
                            | domain.mset.attributes.attrCollection := attrs
                            |]

updateAttributes lvl newAttrs
    [xMatch| [inner] := domain.partition.inner
           | attrs   := domain.partition.attributes.attrCollection
           |] = do
               inner' <- updateAttributes (lvl-1) newAttrs inner
               return [xMake| domain.partition.inner := [inner']
                            | domain.partition.attributes.attrCollection := attrs
                            |]


updateAttributes lvl attrs dom = bug $ vcat [ "don't know how to update this domain"
                                            , pretty lvl
                                            , pretty dom
                                            , pretty attrs
                                            ]


mkAttr :: (T.Text, Maybe E) -> E
mkAttr (n, Nothing) = [xMake| attribute.name.reference := [Prim (S n)] |]
mkAttr (n, Just v ) = [xMake| attribute.nameValue.name.reference := [Prim (S n)]
                            | attribute.nameValue.value          := [v]
                            |]


