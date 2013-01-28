{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.ImplicitWheres
    ( implicitWheres
    , handleInfiniteGivenDoms
    ) where

import Language.E


implicitWheres :: MonadConjure m => Spec -> m Spec
implicitWheres = handleInfiniteGivenDoms

-- replaces all given x : int(1..) with
--              given x : int(1..MAXINT)
handleInfiniteGivenDoms :: MonadConjure m => Spec -> m Spec
handleInfiniteGivenDoms spec
    = fmap (addSupps . second nub)
    $ runWriterT
    $ flip foreachStatement spec $ \ statement ->
        case statement of
            [xMatch| [name]   := topLevel.declaration.given.name
                   | [domain] := topLevel.declaration.given.domain
                   |] -> do
                let (domain', supps) = runWriter $ onDom domain
                if null supps
                    then return [statement]
                    else do
                        let newDecl = [xMake| topLevel.declaration.given.name := [name]
                                            | topLevel.declaration.given.domain := [domain']
                                            |]
                        lift $ mkLog "handleInfDom" $ vcat [ pretty statement
                                                           , "~~>"
                                                           , pretty newDecl
                                                           ]
                        tell supps
                        return [newDecl]
            _ -> return [statement]
    where
        minint     = [xMake| reference := [Prim (S "MININT")] |]
        minintDecl = [xMake| topLevel.declaration.given.name := [minint]
                           | topLevel.declaration.given.domain.domain.int.ranges := []
                           |]
        maxint     = [xMake| reference := [Prim (S "MAXINT")] |]
        maxintDecl = [xMake| topLevel.declaration.given.name := [maxint]
                           | topLevel.declaration.given.domain.domain.int.ranges := []
                           |]

        onRange [xMatch| [f] := range.from |] = ([xMake| range.fromTo := [f,maxint] |], Just maxintDecl)
        onRange [xMatch| [t] := range.to   |] = ([xMake| range.fromTo := [minint,t] |], Just minintDecl)
        onRange r = (r, Nothing)

        onDom d@[xMatch| [r] := domain.int.ranges |] = do
            let (r', maybeSuppDecl) = onRange r
            case maybeSuppDecl of
                Nothing -> return d
                Just suppDecl -> do
                    let newDom  = [xMake| domain.int.ranges := [r'] |]
                    tell [suppDecl]
                    return newDom
        onDom (Tagged t xs) = Tagged t <$> mapM onDom xs
        onDom d = return d

        addSupps (s, []) = s
        addSupps (Spec v s, supps) = Spec v $ listAsStatement $ nub $ supps ++ statementAsList s

