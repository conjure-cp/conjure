{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}

module Language.E.Pipeline.TypeStrengthening ( typeStrengthening ) where

import Language.E
import Language.E.Pipeline.AtMostOneSuchThat ( atMostOneSuchThat )
import Conjure.Bug
-- import Utils.DebugPretty


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
        [xMatch| [name    ] := topLevel.declaration.find.name
               | [D domain] := topLevel.declaration.find.domain
               |] -> do
            let collectedAttributesForThis = [ (lvl, attr, val) | (name', lvl, attr, val) <- collectedAttributes, name == name' ]
            if null collectedAttributesForThis
                then return s
                else do
                    let fs = foldr1 (>=>) [ updateAttributes lvl [mkAttr (Name attr, val)] | (lvl, attr, val) <- collectedAttributesForThis ]
                    domain' <- fs domain
                    return [xMake| topLevel.declaration.find.name   := [name]
                                 | topLevel.declaration.find.domain := [D domain']
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
    :: MonadConjure m => [(E, Domain () E)] -> E
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
    :: MonadConjure m => [(E, Domain () E)] -> E
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
        , Just (DomainPartition _ _ domX) <- x `lookup` findsToConsider
        , dom == D domX
        -> return ([(x, "complete", Nothing)], [])

    -- total function, because every value is assigned

    c@[eMatch| forAll &i : &dom . &x(&j) = &_ |]
        | i == j
        , Just (DomainFunction _ _ domX _) <- x `lookup` findsToConsider
        , dom == D domX
        -> return ([(x, "total", Nothing)], [c])

    -- size, minSize, maxSize

    c@[eMatch| |&x| =  &n |] -> returnIfCat n c ([(x, "size"   , Just n)], [])
    c@[eMatch| |&x| >= &n |] -> returnIfCat n c ([(x, "minSize", Just n)], [])
    c@[eMatch| |&x| <= &n |] -> returnIfCat n c ([(x, "maxSize", Just n)], [])


    -- minOccur, maxOccur

    c@[eMatch| forAll &i : &dom . freq(&x,&j) >= &n |]      | i == j
                                                            , Just (DomainMSet _ _ domX) <- x `lookup` findsToConsider
                                                            , dom == D domX
                                                            -> returnIfCat n c ([(x, "minOccur", Just n)],[])

    c@[eMatch| forAll &i : &dom . freq(&x,&j) <= &n |]      | i == j
                                                            , Just (DomainMSet _ _ domX) <- x `lookup` findsToConsider
                                                            , dom == D domX
                                                            -> returnIfCat n c ([(x, "maxOccur", Just n)],[])

    -- functional, because assigned
    c@[eMatch| forAll &i : &dom . &x(&j,_) = {&_} |]        | i == j
                                                            , Just (DomainRelation _ _ [domX,_]) <- x `lookup` findsToConsider
                                                            , dom == D domX
                                                            -> return ( [ (x, "functional" , Just [eMake| tuple(1) |] )
                                                                        , (x, "total"      , Nothing           )
                                                                        ], [c])
    c@[eMatch| forAll &i : &dom . &x(_,&j) = {&_} |]        | i == j
                                                            , Just (DomainRelation _ _ [_,domX]) <- x `lookup` findsToConsider
                                                            , dom == D domX
                                                            -> return ( [ (x, "functional" , Just [eMake| tuple(2) |] )
                                                                        , (x, "total"      , Nothing           )
                                                                        ], [c])

    c -> relationFunctional findsToConsider c


relationFunctional
    :: MonadConjure m => [(E, Domain () E)] -> E
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
            | Just (DomainRelation _ _ domR) <- r `lookup` findsToConsider
            = do
                -- domR and {dom of args} need to be pair-wise equal
                -- OR underscore.
                -- output is those indices were it is equal.
                let quantifiers = [ i
                                  | (i,a,d) <- zip3 [ 1::Integer .. ] args domR
                                  , a /= [eMake| _ |]
                                  , let aDom = lookup a quans
                                  , Just (D d) == aDom
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
            | Just (DomainRelation _ _ domR) <- r `lookup` findsToConsider
            = do
                -- same as above case, except not `total`
                let quantifiers = [ i
                                  | (i,a,d) <- zip3 [ 1::Integer .. ] args domR
                                  , a /= [eMake| _ |]
                                  , let aDom = lookup a quans
                                  , Just (D d) == aDom
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
        DomainMSet _ (DomainAttributes attrs) inner
            | maxOccur1 `elem` attrs
            -> return $ Just ( name
                             , ( DomainSet () (error "TODO all attrs except maxOccur1 here") inner
                               , [eMake| toMSet(&name) |]
                               ) )
        DomainRelation _ (DomainAttributes attrs) inners
            | functionalAttr'                                    <- "functional" `lookupDomainAttribute` DomainAttributes attrs
            , let functionalAttr = mkAttr ("functional", functionalAttr')
            , Just [xMatch| functionals := value.tuple.values |] <- "functional" `lookupDomainAttribute` DomainAttributes attrs
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
                            _   -> DomainTuple frs
                let to = case tos of
                            [i] -> i
                            _   -> DomainTuple tos
                let permuteTuple = [xMake| value.tuple.values := functionals ++ nonfunctionals |]

                -- wrap in a permute only if it is needed.
                let decorator = if functionals' ++ nonfunctionals' == [1 .. genericLength inners]
                                    then [eMake| toRelation(&name) |]
                                    else [eMake| permute(toRelation(&name),&permuteTuple) |]

                return $ Just ( name
                              , ( DomainFunction () (DomainAttributes (attrs \\ [functionalAttr])) fr to 
                                , decorator
                                ) )
        _ -> return Nothing

    let decorate x | Just (_,y) <- x `lookup` replacements = y
        decorate (Tagged t xs) = Tagged t (map decorate xs)
        decorate x = x

    statements2 <- forM (statementAsList statements1) $ \case
        [xMatch| [name] := topLevel.declaration.find.name
               |] | Just (domain,_) <- name `lookup` replacements
            -> return [xMake| topLevel.declaration.find.name := [name]
                            | topLevel.declaration.find.domain := [D domain]
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


pullFinds :: Spec -> [(E, Domain () E)]
pullFinds (Spec _ x) = mapMaybe pullFind (statementAsList x)
    where pullFind [xMatch| [name]  := topLevel.declaration.find.name
                          | [D dom] := topLevel.declaration.find.domain |] = Just (name, dom)
          pullFind _ = Nothing

updateAttributes
    :: MonadConjure m
    => Int                  -- nesting level, 0 for topmost
    -> [DomainAttribute E]  -- attributes
    -> Domain () E             -- domain
    -> m (Domain () E)         -- modified domain

updateAttributes 0 _newAttrs (DomainSet () _attrs _inner) = error "TODO updateAttributes for DomainSet"
updateAttributes 0 newAttrs (DomainMSet      () (DomainAttributes attrs) inner ) = return $ DomainMSet      () (DomainAttributes (newAttrs ++ attrs)) inner
updateAttributes 0 newAttrs (DomainFunction  () (DomainAttributes attrs) fr to ) = return $ DomainFunction  () (DomainAttributes (newAttrs ++ attrs)) fr to
updateAttributes 0 newAttrs (DomainRelation  () (DomainAttributes attrs) inners) = return $ DomainRelation  () (DomainAttributes (newAttrs ++ attrs)) inners
updateAttributes 0 newAttrs (DomainPartition () (DomainAttributes attrs) inner ) = return $ DomainPartition () (DomainAttributes (newAttrs ++ attrs)) inner

updateAttributes lvl newAttrs (DomainMatrix       index inner) = DomainMatrix       index <$> (updateAttributes (lvl-1) newAttrs inner)
updateAttributes lvl newAttrs (DomainSet       () attrs inner) = DomainSet       () attrs <$> (updateAttributes (lvl-1) newAttrs inner)
updateAttributes lvl newAttrs (DomainMSet      () attrs inner) = DomainMSet      () attrs <$> (updateAttributes (lvl-1) newAttrs inner)
updateAttributes lvl newAttrs (DomainPartition () attrs inner) = DomainPartition () attrs <$> (updateAttributes (lvl-1) newAttrs inner)

updateAttributes lvl attrs dom = bug $ vcat [ "don't know how to update this domain"
                                            , pretty lvl
                                            , pretty dom
                                            , vcat (map pretty attrs)
                                            ]


mkAttr :: (Name, Maybe E) -> DomainAttribute E
mkAttr (n, Nothing) = DAName n
mkAttr (n, Just v ) = DANameValue n v

