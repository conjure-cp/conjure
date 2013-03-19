
-- Given
--     * An Essence spec
--     * An Essence' model for the spec
--     * An EssenceParam file
-- Generate
--     * An Essence'Param file

-- This module is named after the diagram we drew in IanM's room. It
-- probably shouldn't be.

{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.RedArrow ( redArrow ) where


import Language.E

import qualified Data.Text as T



type EssenceSpec = Spec
type Essence'Model = Spec
type Essence'Logs = Text
type EssenceParam = Spec
type Essence'Param = Spec


redArrow
    :: MonadConjure m
    => EssenceSpec -> EssenceParam
    -> Essence'Model -> Essence'Logs
    -> m Essence'Param
redArrow (Spec _ essenceStmt) (Spec _ essenceParamStmt) (Spec langEprime _) modelLogs = do

    forM_ (statementAsList essenceStmt ++ statementAsList essenceParamStmt) $ \ s -> case s of
        [xMatch| [Prim (S nm)] := topLevel.letting.name.reference
               | [value]       := topLevel.letting.expr
               |] -> addReference nm value
        _ -> return ()

    let
        -- Givens in the Essence file
        essenceGivens :: [(Text, E)]
        essenceGivens
            = catMaybes
              [ case full of
                    [xMatch| [Prim (S nm)] := topLevel.declaration.given.name.reference
                           | [dom]         := topLevel.declaration.given.domain
                           |] -> Just (nm, dom)
                    [xMatch| [Prim (S nm)] := topLevel.declaration.given.name.reference
                           |] -> Just (nm, full)
                    _ -> Nothing
              | full <- statementAsList essenceStmt
              ]

    let
        -- Essence Params
        essenceParams :: [(Text, E)]
        essenceParams
            = catMaybes
              [ case full of
                    [xMatch| [Prim (S nm)] := topLevel.letting.name.reference
                           | [val]         := topLevel.letting.expr
                           |] -> Just (nm, val)
                    [xMatch| [Prim (S nm)] := topLevel.letting.name.reference
                           |] -> Just (nm, full)
                    _ -> Nothing
              | full <- statementAsList essenceParamStmt
              ]

    let
        -- Whet representations are we targeting for each given
        lookupReprs :: [(Text, Text)]
        lookupReprs
            = nub
            $ concat
            $ map ( sortBy (comparing fst)
                  . nub
                  . map (\ x -> case identifierSplit x of
                                    (base, _, Just refn) -> (base, refn)
                                    _                    -> error $ "Error parsing the log file: " ++ T.unpack x
                        )
                  . T.splitOn " "
                  . T.strip
                  )
            $ mapMaybe (T.stripPrefix "[configuration]")
            $ T.lines modelLogs

    -- mkLog "debug" $ vcat $
        -- (
            -- "essenceGivens" :
                -- [ nest 4 $ pretty a <+> ":" <+> pretty b
                -- | (a,b) <- essenceGivens
                -- ]
        -- ) ++ (
            -- "essenceParams" :
                -- [ nest 4 $ pretty a <+> ":" <+> pretty b
                -- | (a,b) <- essenceParams
                -- ]
        -- ) ++ (
            -- "lookupReprs"   :
                -- [ nest 4 $ pretty a <+> ":" <+> pretty b
                -- | (a,b) <- lookupReprs
                -- ]
        -- )

    outPairs <- concatMapM (workhorse lookupReprs)
                [ (nm, decl, val)
                | (nm, decl) <- essenceGivens
                , (nm2, val) <- essenceParams
                , nm == nm2
                ]
    let outLettings = [ [xMake| topLevel.letting.name.reference := [Prim (S nm)]
                              | topLevel.letting.expr           := [val]
                              |]
                      | (nm,val) <- outPairs
                      ]

    return (Spec langEprime $ listAsStatement outLettings)


workhorse :: MonadConjure m => [(Text, Text)] -> (Text, E, E) -> m [(Text, E)]
workhorse lookupReprs (nm, dom, val) = do
    let thisReprs = [ repr | (nm', repr) <- lookupReprs, nm == nm' ]
    result <- if null thisReprs
                then helper nm dom val Nothing
                else concatMapM (helper nm dom val . Just) thisReprs
    -- unless (null thisReprs) $
        -- mkLog "debug" $ sep
            -- [ "workhorse"
            -- , "~~" <+> sep (map pretty thisReprs)
            -- , "~~" <+> pretty nm
            -- , "~~" <+> pretty dom
            -- , "~~" <+> pretty val
            -- , "~~" <+> vcat [ "{" <+> pretty i <+> "|" <+> pretty j <+> "}"
                            -- | (i,j) <- result
                            -- ]
            -- ]
    return result

    where

        helper
            :: MonadConjure m
            => Text
            -> E
            -> E
            -> Maybe Text
            -> m [(Text, E)]

        helper
            name
            [xMatch| _ := domain.bool |]
            value
            Nothing
            = return [(name,value)]

        helper
            name
            [xMatch| _ := domain.int |]
            value
            Nothing
            = return [(name,value)]

        helper
            name
            _domain
            [xMatch| enumValues := topLevel.letting.typeEnum.values |]
            Nothing = do
                let outName  = name `T.append` "_fromEnumSize"
                let outValue = [xMake| value.literal := [Prim (I (genericLength enumValues     ))] |]
                return [ (outName, outValue) ]

        helper
            name
            [xMatch| [fr,to]    := domain.set.inner.domain.int.ranges.range.fromTo 
                   | [domInner] := domain.set.inner
                   |]
            [xMatch| values := value.set.values |]
            (Just "Occurrence") = do
            domInner' <- fmap fst $ runWriterT $ fullySimplify domInner
            intFr <- valueIntOut fr
            intTo <- valueIntOut to
            intValues <- mapM valueIntOut values
            let valuesInMatrix = [ if i `elem` intValues then [eMake| true |] else [eMake| false |]
                                 | i <- [intFr .. intTo]
                                 ]
            let theMatrix  = [xMake| value.matrix.values := valuesInMatrix
                                   | value.matrix.indexrange := [domInner']
                                   |]
            let outName = name `T.append` "_Occurrence"
            return [(outName, theMatrix)]

        helper
            name
            [xMatch| attrs      := domain.set.attributes.attrCollection
                   | [domInner] := domain.set.inner
                   |]
            [xMatch| values := value.set.values |]
            (Just "Explicit")
            | let Just size = lookupAttr "size" attrs
            = do
            let indexOfMatrix_fr = [eMake| 1 |]
            let indexOfMatrix_to = [eMake| &size |]
            let indexOfMatrix = [xMake| domain.int.ranges.range.fromTo := [indexOfMatrix_fr,indexOfMatrix_to] |]

            values' <- concatMapM (workhorse lookupReprs)
                        [ (nm', dom', val')
                        | let nm'  = name `T.append` "_Explicit"
                        , let dom' = domInner
                        , val' <- values
                        ]
            let nameValuePairs
                    = map (\ xs -> (fst $ head xs, map snd xs) )
                    $ groupBy ((==) `on` fst)
                    $ sort values'

            return [ (nm', theMatrix)
                   | (nm', vals) <- nameValuePairs
                   , let valuesInMatrix = vals
                   , let theMatrix      = [xMake| value.matrix.values     := valuesInMatrix
                                                | value.matrix.indexrange := [indexOfMatrix]
                                                |]
                   ]

        helper
            name
            [xMatch| attrs      := domain.set.attributes.attrCollection
                   | [domInner] := domain.set.inner
                   |]
            [xMatch| values := value.set.values |]
            (Just "ExplicitVarSize")
            = do
            nbValues <-
                case lookupAttr "maxSize" attrs of
                    Just i  -> return i
                    Nothing -> domSize domInner
            nbValuesInt <- valueIntOut nbValues
            let indexOfMatrix_fr = [eMake| 1 |]
            let indexOfMatrix_to = [xMake| value.literal := [Prim (I nbValuesInt)] |]
            let indexOfMatrix    = [xMake| domain.int.ranges.range.fromTo := [indexOfMatrix_fr,indexOfMatrix_to] |]

            let nbTrues  = genericLength values
            let nbFalses = nbValuesInt - nbTrues
            let outTuple1_Name   = name `T.append` "_ExplicitVarSize_tuple1"
            let outTuple1_Values = replicate (fromInteger nbTrues ) [eMake| true  |]
                                ++ replicate (fromInteger nbFalses) [eMake| false |]
            let outTuple1_Value  = [xMake| value.matrix.values := outTuple1_Values
                                         | value.matrix.indexrange := [indexOfMatrix]
                                         |]


            valuesPadded <- do
                padding <- zeroVal domInner
                return $ values ++ replicate (fromInteger nbFalses) padding
            values' <- concatMapM (workhorse lookupReprs)
                        [ (nm', dom', val')
                        | let nm'  = name `T.append` "_ExplicitVarSize_tuple2"
                        , let dom' = domInner
                        , val' <- valuesPadded
                        ]
            let nameValuePairs
                    = map (\ xs -> (fst $ head xs, map snd xs) )
                    $ groupBy ((==) `on` fst)
                    $ sortBy  (comparing fst)
                    $ values'

            return
                $ (outTuple1_Name, outTuple1_Value)
                :
                [ (nm', theMatrix)
                | (nm', vals) <- nameValuePairs
                , let valuesInMatrix = vals
                , let theMatrix      = [xMake| value.matrix.values     := valuesInMatrix
                                             | value.matrix.indexrange := [indexOfMatrix]
                                             |]
                ]

        helper
            name
            [xMatch| attrs      := domain.set.attributes.attrCollection
                   | [domInner] := domain.set.inner
                   | [fr,_]    := domain.set.inner.domain.int.ranges.range.fromTo
                   |]
            [xMatch| values := value.set.values |]
            (Just "ExplicitVarSizeWithDefault")
            = do

            nbValues <-
                case lookupAttr "maxSize" attrs of
                    Just i  -> return i
                    Nothing -> domSize domInner
            nbValuesInt <- valueIntOut nbValues
            let indexOfMatrix_fr = [eMake| 1 |]
            let indexOfMatrix_to = [xMake| value.literal := [Prim (I nbValuesInt)] |]
            let indexOfMatrix    = [xMake| domain.int.ranges.range.fromTo := [indexOfMatrix_fr,indexOfMatrix_to] |]

            intFr <- valueIntOut fr
            let defValue = [xMake| value.literal := [Prim (I (intFr - 1))] |]

            let nbTrues  = genericLength values
            let nbFalses = nbValuesInt - nbTrues

            let valuesInMatrix = replicate (fromInteger nbFalses) defValue ++ values

            let theMatrix  = [xMake| value.matrix.values := valuesInMatrix
                                   | value.matrix.indexrange := [indexOfMatrix]
                                   |]
            let outName = name `T.append` "_ExplicitVarSizeWithDefault"
            return [(outName, theMatrix)]

        helper name domain value repr = bug $ vcat
            [ "missing case in RedArrow.workhorse"
            , "name:"   <+> pretty name
            , "domain:" <+> pretty domain
            , "value:"  <+> pretty value
            , "repr:"   <+> pretty repr
            ]


valueIntOut :: MonadConjure m => E -> m Integer
valueIntOut [xMatch| [Prim (I x)] := value.literal |] = return x
valueIntOut [xMatch| [Prim (S n)] := reference     |] = do
    mres <- runMaybeT $ lookupReference n
    case mres of
        Just i  -> valueIntOut i
        Nothing -> err ErrFatal $ "No value given for identifier:" <+> pretty n
valueIntOut p = do
    bindersDoc >>= \ d -> mkLog "binders" d
    (p', (Any flag, _)) <- runWriterT $ fullySimplify p
    if flag
        then valueIntOut p'
        else err ErrFatal $ "Expecting integer literal, found:" <+> vcat [ pretty p
                                                                         , prettyAsPaths p
                                                                         ]
-- toInt p            = do
    -- (p', (Any flag, bs)) <- runWriterT $ simplify p
    -- modify $ \ st -> st { binders = bs ++ binders st }
    -- if flag
        -- then do
            -- mres <- toInt p'
            -- case mres of
                -- Nothing         -> return Nothing
                -- Just (p'', bs2) -> return $ Just (p'', bs ++ bs2)
        -- else return Nothing


lookupAttr :: Text -> [E] -> Maybe E
lookupAttr attrName attrs = listToMaybe
    [ val
    | [xMatch| [Prim (S nm)] := attribute.nameValue.name.reference
             | [val]         := attribute.nameValue.value
             |] <- attrs
    , nm == attrName
    ]


zeroVal :: MonadConjure m => E -> m E

zeroVal [xMatch| _     := domain.bool       |] = return [eMake| false |]
zeroVal [xMatch| []    := domain.int.ranges |] = return [eMake| 0 |]
zeroVal [xMatch| (r:_) := domain.int.ranges |] = zeroVal r

zeroVal [xMatch| [x]   := range.single  |] = fmap fst $ runWriterT $ fullySimplify x
zeroVal [xMatch| [x]   := range.from    |] = fmap fst $ runWriterT $ fullySimplify x
zeroVal [xMatch| [x]   := range.to      |] = fmap fst $ runWriterT $ fullySimplify x
zeroVal [xMatch| [x,_] := range.fromTo  |] = fmap fst $ runWriterT $ fullySimplify x

zeroVal [xMatch| [index] := domain.matrix.index
               | [inner] := domain.matrix.inner
               |]
    | [xMatch| [fr,to] := domain.int.ranges.range.fromTo |] <- index
    = do
    intFr <- valueIntOut fr
    intTo <- valueIntOut to
    valInner <- zeroVal inner
    let vals =  replicate (fromInteger $ intTo - intFr + 1) valInner
    return [xMake| value.matrix.values     := vals
                 | value.matrix.indexrange := [index]
                 |]

zeroVal [xMatch| [inner] := domain.set.inner 
               | attrs   := domain.set.attributes.attrCollection
               |]
    | let Just size = lookupAttr "size" attrs
    = do
    sizeInt  <- valueIntOut size
    valInner <- zeroVal inner
    let vals =  replicate (fromInteger sizeInt) valInner
    return [xMake| value.set.values := vals
                 |]
zeroVal x = bug ("RedArrow.zeroVal" <+> prettyAsPaths x)


