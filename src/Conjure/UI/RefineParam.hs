{-# LANGUAGE TupleSections #-}

module Conjure.UI.RefineParam ( refineParam ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.Instantiate
import Conjure.Process.Enums ( removeEnumsFromParam )
import Conjure.Process.FiniteGivens ( finiteGivensParam )
import Conjure.Representations ( downC )


refineParam
    :: ( MonadFail m
       , MonadLog m
       )
    => Model      -- eprime model
    -> Model      -- essence param
    -> m Model    -- eprime param

refineParam eprimeModel essenceParam0 = do

    essenceParam1 <- removeEnumsFromParam eprimeModel essenceParam0
    (essenceParam, generatedLettingNames) <- finiteGivensParam eprimeModel essenceParam1

    logDebug $ "[essenceParam 0]" <+> pretty essenceParam0
    logDebug $ "[essenceParam 1]" <+> pretty essenceParam1
    logDebug $ "[essenceParam 2]" <+> pretty essenceParam

    let essenceLettings   = extractLettings essenceParam
    let essenceGivenNames = eprimeModel |> mInfo |> miGivens
    let essenceGivens     = eprimeModel |> mInfo |> miRepresentations
                                        |> filter (\ (n,_) -> n `elem` essenceGivenNames )


    -- TODO: check if for every given there is a letting (there can be more)
    -- TODO: check if the same letting has multiple values for it

    let eprimeLettingsForEnums =
            [ (nm, fromInt (length vals))
            | nm1                                          <- eprimeModel |> mInfo |> miEnumGivens
            , Declaration (LettingDomainDefnEnum nm2 vals) <- essenceParam0 |> mStatements
            , nm1 == nm2
            , let nm = nm1 `mappend` "_EnumSize"
            ]

    essenceLettings' <- forM essenceLettings $ \ (name, val) -> do
        constant <- instantiateExpression (essenceLettings ++ map (second Constant) eprimeLettingsForEnums) val
        return (name, constant)

    essenceGivens' <- forM essenceGivens $ \ (name, dom) -> do
        constant <- instantiateDomain (essenceLettings ++ map (second Constant) eprimeLettingsForEnums) dom
        return (name, constant)

    let extraLettings = map fst essenceLettings' \\
                        (map fst essenceGivens' ++ generatedLettingNames)

    unless (null extraLettings) $
        logWarn ("Extra lettings:" <+> prettyList id "," extraLettings)

    let essenceGivensAndLettings = catMaybes
            [ case lookup n essenceLettings' of
                Nothing ->
                    if n `elem` map fst eprimeLettingsForEnums
                        then Nothing
                        else userErr $ vcat
                                [ "No value for parameter:" <+> pretty n
                                , "With domain:" <+> pretty d
                                ]
                Just v  -> Just (n, d, v)
            | (n, d) <- essenceGivens' ++ map (,DomainInt []) generatedLettingNames
            ]

    let f (Reference nm Nothing) =
            case [ val | (nm2, val) <- eprimeLettingsForEnums, nm == nm2 ] of
                []    -> bug ("refineParam: No value for" <+> pretty nm)
                [val] -> Constant val
                _     -> bug ("refineParam: Multiple values for" <+> pretty nm)
        f p = p

    let essenceGivensAndLettings' = transformBi f essenceGivensAndLettings
    eprimeLettings <- liftM concat $ mapM downC essenceGivensAndLettings'

    return $ languageEprime def
        { mStatements =
            [ Declaration (Letting n (Constant x))
            | (n, _, x) <- eprimeLettings
            ] ++
            [ Declaration (Letting n (Constant x))
            | (n, x) <- eprimeLettingsForEnums
            ]
        }
