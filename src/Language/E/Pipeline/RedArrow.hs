{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}

module Language.E.Pipeline.RedArrow ( redArrow ) where


import Language.E

import qualified Data.Text as T
import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M


-- Given
--     * An Essence spec
--     * An Essence' model for the spec
--     * A  EssenceParam file
-- Generate
--     * A Essence'Param file

-- This module is named after the diagram we drew in IanM's room. It
-- probably shouldn't be.

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
redArrow spec specParam@(Spec _ specParamStatements) _model@(Spec langEprime _) modelLogs = do

    ints <- execWriterT $ forM_ (statementAsList specParamStatements) $ \ s -> case lettingOut s of
        Nothing -> return ()
        Just (nm, _) ->
            if nm `elem` map fst (concat eprimeReprs)
                then return ()
                else tell [s]

    enums <- execWriterT $ forM_ (statementAsList specParamStatements) $ \ s -> case s of
        [xMatch| [Prim (S nm)] := topLevel.letting.name.reference
               | enumValues    := topLevel.letting.typeEnum.values
               |] -> do
                let outName  = [xMake| reference     := [Prim (S (nm `T.append` "_fromEnumSize"))] |]
                let outValue = [xMake| value.literal := [Prim (I (genericLength enumValues     ))] |]
                let out      = [xMake| topLevel.letting.name := [outName]
                                     | topLevel.letting.expr := [outValue]
                                     |]
                tell [out]
        _ -> return ()

    abstracts <- execWriterT $ withBindingScope $ do
        lift $ mapM_ introduceStuff (statementAsList specParamStatements)
        case eprimeReprs of
            [] -> return ()
            [eprimeRepr] ->
                forM_ eprimeRepr $ \ (name, reprName) ->
                    unless (S.member name specFinds) $
                        case (M.lookup name specGivens, M.lookup name paramLettings) of
                            (Just domain, Just value) -> do
                                xs <- lift $ onSingleDom reprName name domain value
                                tell xs
                            (_, Nothing) -> lift $ err ErrFatal $ "No value given for parameter:" <+> pretty name
                            (Nothing, _) -> lift $ err ErrFatal $ "Unknown parameter in the configuration file:" <+> pretty name
            _ -> lift $ mkLog "warning" "Don't know what to do with this logs file, sorry."

    return (Spec langEprime $ listAsStatement $ ints ++ enums ++ abstracts)

    where

        -- The finds in the input Essence
        specFinds :: S.HashSet Text
        specFinds
            = S.fromList
            $ mapMaybe findOut
            $ (\ (Spec _ x) -> statementAsList x ) spec

        -- The givens in the input Essence
        specGivens :: M.HashMap Text E
        specGivens
            = M.fromList
            $ mapMaybe givenOut
            $ (\ (Spec _ x) -> statementAsList x ) spec

        -- The params in the Essence param file
        paramLettings :: M.HashMap Text E
        paramLettings
            = M.fromList
            $ mapMaybe lettingOut
            $ (\ (Spec _ x) -> statementAsList x) specParam

        -- Whet representations are we targeting for each given
        eprimeReprs :: [[(Text,Text)]]
        eprimeReprs
            = map ( sortBy (comparing fst)
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

        findOut :: E -> Maybe Text
        findOut [xMatch| [Prim (S nm)] := topLevel.declaration.find.name.reference
                        |] = Just nm
        findOut _ = Nothing

        givenOut :: E -> Maybe (Text, E)
        givenOut [xMatch| [Prim (S nm)] := topLevel.declaration.given.name.reference
                        | [domain]      := topLevel.declaration.given.domain
                        |] = Just (nm, domain)
        givenOut _ = Nothing

        lettingOut :: E -> Maybe (Text, E)
        lettingOut [xMatch| [Prim (S nm)] := topLevel.letting.name.reference
                          | [expr]        := topLevel.letting.expr
                          |] = Just (nm, expr)
        lettingOut _ = Nothing


onSingleDom
    :: MonadConjure m
    => Text                 -- representation name
    -> Text                 -- (name   of) the original declaration 
    -> E                    -- (domain of) the original declaration
    -> E                    -- (value  of) param instantiating the original declaration
    -> m [E]                -- params instantiating the refined declaration
onSingleDom "Occurrence"
    name
    [xMatch| [fr,to]    := domain.set.inner.domain.int.ranges.range.fromTo 
           | [innerDom] := domain.set.inner
           |]
    [xMatch| values := value.set.values |] = do
    frInt <- valueIntOut fr
    toInt <- valueIntOut to
    intValues <- mapM valueIntOut values
    let valuesInMatrix = [ if i `elem` intValues then [eMake| 1 |] else [eMake| 0 |]
                         | i <- [frInt..toInt]
                         ]
    let theMatrix  = [xMake| value.matrix.values := valuesInMatrix
                           | value.matrix.indexrange := [innerDom]
                           |]
    let outName = name `T.append` "_Occurrence"
    let theLetting = [xMake| topLevel.letting.name.reference := [Prim (S outName)]
                           | topLevel.letting.expr           := [theMatrix]
                           |]
    return [theLetting]
onSingleDom "ExplicitVarSize"
    name
    [xMatch| [fr,to]    := domain.set.inner.domain.int.ranges.range.fromTo
           | attrs      := domain.set.attributes.attrCollection
           |]
    [xMatch| values := value.set.values |] = do
    frInt <- valueIntOut fr
    toInt <- valueIntOut to
    intValues <- fmap sort $ mapM valueIntOut values

    nbValues <- case attrs of
        [] -> return (toInt - frInt + 1)
        _  -> err ErrFatal "Error while refining parameters"
    let nbTrues  = genericLength intValues
    let nbFalses = nbValues - nbTrues

    let indexOfMatrix_fr = [eMake| 1 |]
    let indexOfMatrix_to = [xMake| value.literal := [Prim (I nbValues)] |]
    let indexOfMatrix = [xMake| domain.int.ranges.range.fromTo := [indexOfMatrix_fr,indexOfMatrix_to] |]

    let outTuple1_Name   = name `T.append` "_ExplicitVarSize_tuple1"
    let outTuple1_Values = replicate (fromInteger nbTrues ) [eMake| true  |]
                        ++ replicate (fromInteger nbFalses) [eMake| false |]
    let outTuple1_Value  = [xMake| value.matrix.values := outTuple1_Values
                                 | value.matrix.indexrange := [indexOfMatrix]
                                 |]
    let outTuple1        = [xMake| topLevel.letting.name.reference := [Prim (S outTuple1_Name)]
                                 | topLevel.letting.expr           := [outTuple1_Value]
                                 |]

    let outTuple2_Name   = name `T.append` "_ExplicitVarSize_tuple2"
    let outTuple2_Values = map (\ x -> [xMake| value.literal := [Prim (I x)] |] ) intValues
                        ++ replicate (fromInteger nbFalses) [xMake| value.literal := [Prim (I frInt)] |]
    let outTuple2_Value  = [xMake| value.matrix.values := outTuple2_Values
                                 | value.matrix.indexrange := [indexOfMatrix]
                                 |]
    let outTuple2        = [xMake| topLevel.letting.name.reference := [Prim (S outTuple2_Name)]
                                 | topLevel.letting.expr           := [outTuple2_Value]
                                 |]

    return [outTuple1, outTuple2]
onSingleDom "Explicit"
    name
    [xMatch| attrs := domain.set.attributes.attrCollection |]
    [xMatch| values := value.set.values |]
    | let Just size = listToMaybe
                        [ s
                        | [xMatch| [Prim (S "size")] := attribute.nameValue.name.reference
                                 | [s]               := attribute.nameValue.value
                                 |] <- attrs
                        ]
    = do
    let valuesInMatrix = sort values
    let indexOfMatrix_fr = [eMake| 1 |]
    let indexOfMatrix_to = [eMake| &size |]
    let indexOfMatrix = [xMake| domain.int.ranges.range.fromTo := [indexOfMatrix_fr,indexOfMatrix_to] |]
    let theMatrix  = [xMake| value.matrix.values := valuesInMatrix
                           | value.matrix.indexrange := [indexOfMatrix]
                           |]
    let outName = name `T.append` "_Explicit"
    let theLetting = [xMake| topLevel.letting.name.reference := [Prim (S outName)]
                           | topLevel.letting.expr           := [theMatrix]
                           |]
    return [theLetting]
onSingleDom reprName name domain value
    = err ErrFatal $ "missing: onSingleDom" <+> vcat [ pretty name
                                                     , pretty reprName
                                                     , pretty domain
                                                     , pretty value
                                                     ]


valueIntOut :: MonadConjure m => E -> m Integer
valueIntOut [xMatch| [Prim (I x)] := value.literal |] = return x
valueIntOut [xMatch| [Prim (S nm)] := reference    |] = do
    mx <- runMaybeT $ lookupReference nm
    case mx of
        Nothing -> err ErrFatal $ "Parameter value not given for:" <+> pretty nm
        Just x  -> valueIntOut x
valueIntOut p = err ErrFatal $ "Expecting integer literal, found:" <+> pretty p

