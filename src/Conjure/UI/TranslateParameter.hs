module Conjure.UI.TranslateParameter ( translateParameter ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Constant
import Conjure.Language.Type
import Conjure.Language.TypeOf
import Conjure.Language.Pretty
import Conjure.Language.Instantiate
import Conjure.Process.Enums ( removeEnumsFromParam )
import Conjure.Process.FiniteGivens ( finiteGivensParam )
import Conjure.Process.Enumerate ( EnumerateDomain )
import Conjure.Representations ( downC )


translateParameter ::
    MonadFail m =>
    MonadLog m =>
    NameGen m =>
    EnumerateDomain m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model ->     -- eprime model
    Model ->     -- essence param
    m Model      -- eprime param

translateParameter eprimeModel0 essenceParam0 = do
    logDebug $ "[eprimeModel  0]" <+> pretty essenceParam0
    logDebug $ "[essenceParam 0]" <+> pretty essenceParam0
    (eprimeModel, essenceParam1) <- removeEnumsFromParam eprimeModel0 essenceParam0
    logDebug $ "[eprimeModel  1]" <+> pretty eprimeModel
    logDebug $ "[essenceParam 1]" <+> pretty essenceParam1
    (essenceParam, generatedLettingNames) <- finiteGivensParam eprimeModel essenceParam1
    logDebug $ "[essenceParam 2]" <+> pretty essenceParam

    let essenceLettings   = extractLettings essenceParam
    let essenceGivenNames = eprimeModel |> mInfo |> miGivens
    let essenceGivens     = eprimeModel |> mInfo |> miRepresentations
                                        |> filter (\ (n,_) -> n `elem` essenceGivenNames )

    logDebug $ "[essenceLettings  ]" <+> vcat [ pretty n <> ":" <+> pretty x | (n,x) <- essenceLettings ]
    logDebug $ "[essenceGivenNames]" <+> vcat (map pretty essenceGivenNames)
    logDebug $ "[essenceGivens    ]" <+> vcat [ pretty n <> ":" <+> pretty x | (n,x) <- essenceGivens ]

    -- some sanity checks here
    -- TODO: check if for every given there is a letting (there can be more)
    -- TODO: check if the same letting has multiple values for it
    let missingLettings =
            (essenceGivenNames ++ generatedLettingNames) \\
            map fst essenceLettings
    unless (null missingLettings) $
        userErr1 $ "Missing values for parameters:" <++> prettyList id "," missingLettings

    let extraLettings =
            map fst essenceLettings \\
            (essenceGivenNames ++ generatedLettingNames)
    unless (null extraLettings) $
        userErr1 $ "Too many letting statements in the parameter file:" <++> prettyList id "," extraLettings


    let eprimeLettingsForEnums =
            [ (nm, fromInt (genericLength vals))
            | nm1                                          <- eprimeModel |> mInfo |> miEnumGivens
            , Declaration (LettingDomainDefnEnum nm2 vals) <- essenceParam0 |> mStatements
            , nm1 == nm2
            , let nm = nm1 `mappend` "_EnumSize"
            ]

    let allLettings = (eprimeModel |> mInfo |> miLettings)
                   ++ essenceLettings
                   ++ map (second Constant) eprimeLettingsForEnums

    essenceLettings' <- forM essenceLettings $ \ (name, val) -> do
        constant <- instantiateExpression allLettings val
        return (name, constant)
    logDebug $ "[essenceLettings' ]" <+> vcat [ pretty n <> ":" <+> pretty x | (n,x) <- essenceLettings' ]

    essenceGivens' <- forM essenceGivens $ \ (name, dom) -> do
        constant <- instantiateDomain allLettings dom
        return (name, constant)
    logDebug $ "[essenceGivens'   ]" <+> vcat [ pretty n <> ":" <+> pretty x | (n,x) <- essenceGivens' ]

    essenceGivensAndLettings <- sequence
            [ case lookup n essenceLettings' of
                Nothing ->
                    if n `elem` map fst eprimeLettingsForEnums
                        then return Nothing
                        else userErr1 $ vcat
                                [ "No value for parameter:" <+> pretty n
                                , "With domain:" <+> pretty d
                                ]
                Just v  ->
                    if emptyCollection v
                        then do
                            (c, cTyMaybe) <- case v of
                                TypedConstant c cTy
                                    | elem TypeAny (universe cTy)       -- we may be able to do better!
                                        -> return (c, Just cTy)
                                    | otherwise
                                        -> return (v, Nothing)          -- already sufficiently typed
                                _ -> return (v, Just TypeAny)           -- empty collection, unknown type
                            case cTyMaybe of
                                Nothing -> return $ Just (n, d, v)
                                Just cTy1 -> do
                                    -- calculate the type of the domain, unify with the type we already have
                                    cTy2 <- typeOf d
                                    let cTy = mostDefined [cTy1, cTy2]
                                    if elem TypeAny (universe cTy)
                                        then userErr1 $ vcat
                                            [ "Cannot fully determine the type of parameter" <+> pretty n
                                            , "Domain:" <+> pretty d
                                            , "Value :" <+> pretty v
                                            ]
                                        else return $ Just (n, d, TypedConstant c cTy)
                        else return $ Just (n, d, v)
            | (n, d) <- essenceGivens' ++ [ (n, DomainInt TagInt []) | n <- generatedLettingNames ]
            ]
    logDebug $ "[essenceGivensAndLettings ]" <+> vcat [ vcat [ "name    :" <+> pretty n
                                                             , "domain  :" <+> pretty d
                                                             , "constant:" <+> pretty c
                                                             ]
                                                      | Just (n,d,c) <- essenceGivensAndLettings
                                                      ]

    let f (Reference nm Nothing) =
            case [ val | (nm2, val) <- eprimeLettingsForEnums, nm == nm2 ] of
                []    -> bug ("translateParameter: No value for" <+> pretty nm)
                [val] -> Constant val
                _     -> bug ("translateParameter: Multiple values for" <+> pretty nm)
        f p = p

    let
        essenceGivensAndLettings' :: [(Name, Domain HasRepresentation Constant, Constant)]
        essenceGivensAndLettings' = transformBi f (catMaybes essenceGivensAndLettings)

    logDebug $ "[essenceGivensAndLettings']" <+> vcat [ vcat [ "name    :" <+> pretty n
                                                             , "domain  :" <+> pretty d
                                                             , "constant:" <+> pretty c
                                                             ]
                                                      | (n,d,c) <- essenceGivensAndLettings'
                                                      ]

    errs <- execWriterT $ forM_ essenceGivensAndLettings' $ \ (nm, dom, val) ->
        case validateConstantForDomain nm val dom of
            Left err -> tell [err]
            Right () -> return ()
    unless (null errs) (userErr errs)

    let
        decorateWithType p@(_, _, TypedConstant{}) = return p
        decorateWithType (name, domain, constant) | emptyCollection constant = do
            ty <- typeOf domain
            return (name, domain, TypedConstant constant ty)
        decorateWithType p = return p

    eprimeLettings
        :: [(Name, Domain HasRepresentation Constant, Constant)]
        <- failToUserError $ concatMapM downC essenceGivensAndLettings' >>= mapM decorateWithType
    logDebug $ "[eprimeLettings           ]" <+> vcat [ vcat [ "name    :" <+> pretty n
                                                             , "domain  :" <+> pretty d
                                                             , "constant:" <+> pretty c
                                                             ]
                                                      | (n,d,c) <- eprimeLettings
                                                      ]

    return $ languageEprime def
        { mStatements =
            [ Declaration (Letting n (Constant x))
            | (n, _, x) <- eprimeLettings
            ] ++
            [ Declaration (Letting n (Constant x))
            | (n, x) <- eprimeLettingsForEnums
            ]
        }
