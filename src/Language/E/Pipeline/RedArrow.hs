
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


import Bug
import Language.E
import Language.E.Pipeline.Groom ( groomSpec )

import qualified Data.Text as T
import Data.List ( findIndex, transpose )



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

    forM_ (statementAsList essenceStmt ++ statementAsList essenceParamStmt) $ \ s -> do
        introduceStuff s
        case s of
            [xMatch| [Prim (S nm)] := topLevel.letting.name.reference
                   | [value]       := topLevel.letting.expr
                   |] -> addReference nm value
            [xMatch| [Prim (S nm)] := topLevel.letting.name.reference
                   |] -> addReference nm s
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

    -- bs <- bindersDoc
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
        -- ) ++ [bs]

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

    groomSpec False (Spec langEprime $ listAsStatement outLettings)


workhorse :: MonadConjure m => [(Text, Text)] -> (Text, E, E) -> m [(Text, E)]
workhorse lookupReprs (nm, domBefore, valBefore) = do
    dom <- instantiate [] domBefore
    val <- instantiate [] valBefore
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

        callHelper name _      val@[xMatch| _ := value.literal |] _ = return [(name, val)]
        callHelper name domain val repr = helper name domain val repr

        helper
            :: MonadConjure m
            => Text
            -> E
            -> E
            -> Maybe Text
            -> m [(Text, E)]

        -- helper name domain value _
            -- | trace (show $ sep [ "helper"
                                -- , pretty name
                                -- , pretty domain
                                -- , pretty value
                                -- ]) False = undefined

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
            _domain
            value@[xMatch| _ := value.matrix.values |]
            Nothing = do
                return [(name,value)]

        helper
            name
            [xMatch| [fr,to]    := domain.set.inner.domain.int.ranges.range.fromTo 
                   | [domInner] := domain.set.inner
                   |]
            [xMatch| values := value.set.values |]
            (Just "Set~Occurrence") = do
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
            let outName = name `T.append` "_Set~Occurrence"
            return [(outName, theMatrix)]

        helper
            name
            [xMatch| attrs      := domain.set.attributes.attrCollection
                   | [domInner] := domain.set.inner
                   |]
            [xMatch| values := value.set.values |]
            (Just "Set~Explicit")
            | Just size <- lookupAttr "size" attrs
            = do
            sizeInt <- valueIntOut size
            let indexOfMatrix_fr = [eMake| 1 |]
            let indexOfMatrix_to = [xMake| value.literal := [Prim (I sizeInt)] |]
            let indexOfMatrix = [xMake| domain.int.ranges.range.fromTo := [indexOfMatrix_fr,indexOfMatrix_to] |]

            values' <- concatMapM (workhorse lookupReprs)
                        [ (nm', dom', val')
                        | let nm'  = name `T.append` "_Set~Explicit"
                        , let dom' = domInner
                        , val' <- values
                        ]
            let nameValuePairs
                    = map (\ xs -> (fst $ headNote "redArrow.nameValuePairs 1" xs, map snd xs) )
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
            (Just "Set~ExplicitVarSize")
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
            let outTuple1_Name   = name `T.append` "_Set~ExplicitVarSize_tuple1"
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
                        | let nm'  = name `T.append` "_Set~ExplicitVarSize_tuple2"
                        , let dom' = domInner
                        , val' <- valuesPadded
                        ]
            let nameValuePairs
                    = map (\ xs -> (fst $ headNote "redArrow.nameValuePairs 2" xs, map snd xs) )
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
                   |]
            [xMatch| values := value.set.values |]
            (Just "Set~ExplicitVarSizeWithMarker")
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
            let outTuple1_Name   = name `T.append` "_Set~ExplicitVarSizeWithMarker_tuple1"
            let outTuple1_Value  = [xMake| value.literal := [Prim (I nbTrues)] |]

            valuesPadded <- do
                padding <- zeroVal domInner
                return $ sort values ++ replicate (fromInteger nbFalses) padding
            values' <- concatMapM (workhorse lookupReprs)
                        [ (nm', dom', val')
                        | let nm'  = name `T.append` "_Set~ExplicitVarSizeWithMarker_tuple2"
                        , let dom' = domInner
                        , val' <- valuesPadded
                        ]
            let nameValuePairs
                    = map (\ xs -> (fst $ headNote "redArrow.nameValuePairs 2" xs, map snd xs) )
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
                   | [_,to]     := domain.set.inner.domain.int.ranges.range.fromTo
                   |]
            [xMatch| values := value.set.values |]
            (Just "Set~ExplicitVarSizeWithDefault")
            = do

            nbValues <-
                case lookupAttr "maxSize" attrs of
                    Just i  -> return i
                    Nothing -> domSize domInner
            nbValuesInt <- valueIntOut nbValues
            let indexOfMatrix_fr = [eMake| 1 |]
            let indexOfMatrix_to = [xMake| value.literal := [Prim (I nbValuesInt)] |]
            let indexOfMatrix    = [xMake| domain.int.ranges.range.fromTo := [indexOfMatrix_fr,indexOfMatrix_to] |]

            intTo <- valueIntOut to
            let defValue = [xMake| value.literal := [Prim (I (intTo + 1))] |]

            let nbTrues  = genericLength values
            let nbFalses = nbValuesInt - nbTrues

            let valuesInMatrix = sort values ++ replicate (fromInteger nbFalses) defValue

            let theMatrix  = [xMake| value.matrix.values := valuesInMatrix
                                   | value.matrix.indexrange := [indexOfMatrix]
                                   |]
            let outName = name `T.append` "_Set~ExplicitVarSizeWithDefault"
            return [(outName, theMatrix)]

        helper
            name
            [xMatch| attrs        := domain.function.attributes.attrCollection
                   | [domInnerFr] := domain.function.innerFrom
                   | [domInnerTo] := domain.function.innerTo
                   |]
            [xMatch| values := value.function.values |]
            (Just "AsReln")
            = do
                let
                    mappingToTuple [xMatch| [a,b] := mapping |] = [xMake| value.tuple.values := [a,b] |]
                    mappingToTuple p = bug $ vcat [ "workhorse.helper.AsReln 1", pretty p ]
                    valuesOut = map mappingToTuple values
                    nameOut = name `T.append` "_AsReln"
                case lookup nameOut lookupReprs of
                    Nothing   -> bug $ vcat [ "workhorse.helper.AsReln 2", pretty name]
                    Just repr ->
                        helper
                            nameOut
                            [xMake| domain.relation.attributes.attrCollection := attrs
                                  | domain.relation.inners := [domInnerFr, domInnerTo]
                                  |]
                            [xMake| value.relation.values := valuesOut |]
                            (Just repr)

        helper
            name
            [xMatch| [domInnerFr] := domain.function.innerFrom |]
            [xMatch| values := value.function.values |]
            (Just "Matrix1D")
            = do
                let
                    mappingToTuple [xMatch| [a,b] := mapping |] = (a,b)
                    mappingToTuple p = bug $ vcat [ "workhorse.helper.Matrix1D", pretty p ]
                    (_indexValues, actualValues) = unzip $ sortBy (comparing fst) $ map mappingToTuple values
                    nameOut = name `T.append` "_Matrix1D"
                domInnerFr' <- instantiate [] domInnerFr
                let
                    valueOut = [xMake| value.matrix.values     := actualValues
                                     | value.matrix.indexrange := [domInnerFr']
                                     |]
                return [(nameOut, valueOut)]

        helper
            name
            [xMatch| attrs     := domain.relation.attributes.attrCollection
                   | domInners := domain.relation.inners
                   |]
            [xMatch| values := value.relation.values |]
            (Just "RelationAsSet")
            = do
                let
                    nameOut = name `T.append` "_RelationAsSet"
                    domInnerOut = [xMake| domain.tuple.inners := domInners |]
                case lookup nameOut lookupReprs of
                    Nothing   -> bug $ vcat [ "workhorse.helper.RelationAsSet", pretty name]
                    Just repr ->
                        helper
                            nameOut
                            [xMake| domain.set.attributes.attrCollection := attrs
                                  | domain.set.inner := [domInnerOut]
                                  |]
                            [xMake| value.set.values := values |]
                            (Just repr)

        helper
            name
            [xMatch| [da,db] := domain.relation.inners |]
            [xMatch| values  := value.relation.values |]
            (Just "RelationIntMatrix2")
            = do
                da' <- instantiate [] da
                db' <- instantiate [] db
                let nameOut = name `T.append` "_RelationIntMatrix2"
                case (da', db') of
                    (  [xMatch| [aFr,aTo] := domain.int.ranges.range.fromTo |]
                     , [xMatch| [bFr,bTo] := domain.int.ranges.range.fromTo |]
                      ) -> do
                        aFr' <- valueIntOut aFr
                        aTo' <- valueIntOut aTo
                        bFr' <- valueIntOut bFr
                        bTo' <- valueIntOut bTo
                        let outMatrix = [ [ [xMake| value.literal := [Prim (B $ elem forlookup values)] |]
                                          | i <- [ aFr' .. aTo' ]
                                          , let forlookup_1 = [xMake| value.literal := [Prim (I i)] |]
                                          , let forlookup_2 = [xMake| value.literal := [Prim (I j)] |]
                                          , let forlookup = [xMake| value.tuple.values := [forlookup_1, forlookup_2] |]
                                          ]
                                        | j <- [ bFr' .. bTo' ]
                                        ]
                        let valueMatrix xs = [xMake| value.matrix.values := xs |]
                        let outMatrix' = valueMatrix $ map valueMatrix $ transpose outMatrix
                        return [(nameOut, outMatrix')]
                    (_, _) -> bug $ vcat [ "workhorse.helper.RelationIntMatrix2", pretty name]

        helper
            name
            [xMatch| ds := domain.tuple.inners |]
            [xMatch| vs := value.tuple.values  |]
            Nothing | length ds == length vs = do
                let outNames = [ mconcat [name, "_tuple", T.pack (show i)]
                               | i <- [1 .. length ds]
                               ]
                liftM concat $ sequence [ callHelper n d v Nothing
                                        | (n,d,v) <- zip3 outNames ds vs
                                        ]

        helper
            name
            [xMatch| [Prim (S domId)] := reference |]
            value
            Nothing = do
                domain <- runMaybeT $ lookupReference domId
                case domain of
                    Just [xMatch| vs := topLevel.letting.typeEnum.values |] ->
                        case findIndex (== value) vs of
                            Nothing -> userErr $ vcat
                                [ "Not an element of the enumeration:" <+> pretty value
                                , "Options were:" <+> fsep (map pretty vs)
                                ]
                            Just i  -> return [(name, [xMake| value.literal := [Prim $ I $ fromIntegral $ i + 1] |])]
                    _ -> bug "don't know what this is"

        helper name domain value Nothing =
            case lookup name lookupReprs of
                Just repr -> helper name domain value (Just repr)
                Nothing -> bug $ vcat
                    [ "missing case in RedArrow.workhorse"
                    , "name:"   <+> pretty name
                    , "domain:" <+> vcat (map ($ domain) [pretty, prettyAsPaths])
                    , "value:"  <+> vcat (map ($ value ) [pretty, prettyAsPaths])
                    ]

        helper name domain value repr = bug $ vcat
            [ "missing case in RedArrow.workhorse"
            , "name:"   <+> pretty name
            , "domain:" <+> vcat (map ($ domain) [pretty, prettyAsPaths])
            , "value:"  <+> vcat (map ($ value ) [pretty, prettyAsPaths])
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
    (p', (Any flag, _)) <- runWriterT $ fullySimplify p
    if flag
        then valueIntOut p'
        else err ErrFatal $ "Expecting integer literal, found:" <+> vcat [ pretty p
                                                                         , prettyAsPaths p
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
    | Just size <- lookupAttr "size" attrs
    = do
    sizeInt  <- valueIntOut size
    valInner <- zeroVal inner
    let vals =  replicate (fromInteger sizeInt) valInner
    return [xMake| value.set.values := vals
                 |]

zeroVal [xMatch| xs := domain.tuple.inners |] = do
    zeroes <- mapM zeroVal xs
    return [xMake| value.tuple.values := zeroes |]

zeroVal p@[xMatch| [Prim (S domId)] := reference |] = do
    domain <- runMaybeT $ lookupReference domId
    case domain of
        Just [xMatch| vs := topLevel.letting.typeEnum.values |] ->
            case vs of
                (v:_) -> return v
                _     -> userErr $ "Empty enumeration:" <+> pretty domId
        _ -> do
            x <- instantiate [] p
            zeroVal x

zeroVal x = bug ("RedArrow.zeroVal" <+> prettyAsPaths x)


instantiate :: MonadConjure m => [Text] -> E -> m E
instantiate seen [xMatch| [Prim (S domId)] := reference |]
    | domId `elem` seen = userErr $ "Cyclic definition:" <+> fsep (map pretty seen)
instantiate seen p@[xMatch| [Prim (S domId)] := reference |] = do
    mdomain <- runMaybeT $ lookupReference domId
    case mdomain of
        Nothing -> return p
        Just [xMatch| _        := topLevel.letting.typeEnum |] -> return p
        Just [xMatch| [domain] := topLevel.letting.domain   |] -> instantiate (domId:seen) domain
        Just domain -> instantiate (domId:seen) domain
instantiate seen (Tagged t xs) = Tagged t <$> mapM (instantiate seen) xs
instantiate _ p = return p

