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
redArrow spec specParam _model@(Spec langEprime _) modelLogs = do
    -- mkLog "debug specGivens"    (vcat $ map pretty $ M.toList specGivens)
    -- mkLog "debug paramLettings" (vcat $ map pretty $ M.toList paramLettings)
    -- forM_ eprimeReprs $ \ x ->
        -- mkLog "debug eprimeReprs"   (vcat $ map pretty x)

    case eprimeReprs of
        [] -> return specParam
        [eprimeRepr] -> do
            xs <- forM eprimeRepr $ \ (name, reprName) ->
                if S.member name specFinds
                    then return []
                    else
                        case (M.lookup name specGivens, M.lookup name paramLettings) of
                            (Just domain, Just value) -> onSingleDom reprName name domain value
                            (_, Nothing) -> err ErrFatal $ "No value given for parameter:" <+> pretty name
                            (Nothing, _) -> err ErrFatal $ "Unknown parameter in the configuration file:" <+> pretty name
            return (Spec langEprime $ listAsStatement $ concat xs)
        _ -> error "Don't know what to do with this logs file, sorry."

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
           | [innerDom] := domain.set.inner
           |]
    [xMatch| values := value.set.values |] = do
    frInt <- valueIntOut fr
    toInt <- valueIntOut to
    intValues <- fmap sort $ mapM valueIntOut values

    let nbTrues  = genericLength intValues
    let nbFalses = toInt - frInt + 1 - nbTrues

    let outTuple1_Name   = name `T.append` "_ExplicitVarSize_tuple1"
    let outTuple1_Values = replicate (fromInteger nbTrues ) [eMake| true  |]
                        ++ replicate (fromInteger nbFalses) [eMake| false |]
    let outTuple1_Value  = [xMake| value.matrix.values := outTuple1_Values
                                 | value.matrix.indexrange := [innerDom]
                                 |]
    let outTuple1        = [xMake| topLevel.letting.name.reference := [Prim (S outTuple1_Name)]
                                 | topLevel.letting.expr           := [outTuple1_Value]
                                 |]

    let outTuple2_Name   = name `T.append` "_ExplicitVarSize_tuple2"
    let outTuple2_Values = map (\ x -> [xMake| value.literal := [Prim (I x)] |] ) intValues
                        ++ replicate (fromInteger nbFalses) [eMake| 0 |]
    let outTuple2_Value  = [xMake| value.matrix.values := outTuple2_Values
                                 | value.matrix.indexrange := [innerDom]
                                 |]
    let outTuple2        = [xMake| topLevel.letting.name.reference := [Prim (S outTuple2_Name)]
                                 | topLevel.letting.expr           := [outTuple2_Value]
                                 |]

    return [outTuple1, outTuple2]
onSingleDom "Explicit"
    name
    [xMatch| [innerDom] := domain.set.inner |]
    [xMatch| values := value.set.values |] = do
    let valuesInMatrix = sort values
    let theMatrix  = [xMake| value.matrix.values := valuesInMatrix
                           | value.matrix.indexrange := [innerDom]
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
valueIntOut p = err ErrFatal $ "Expecting integer literal, found:" <+> pretty p

