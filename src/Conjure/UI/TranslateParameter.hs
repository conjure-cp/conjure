module Conjure.UI.TranslateParameter ( translateParameter ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Constant
import Conjure.Language.Type
import Conjure.Language.Pretty
import Conjure.Language.Instantiate
import Conjure.Process.Enums ( removeEnumsFromParam )
import Conjure.Process.FiniteGivens ( finiteGivensParam )
import Conjure.Process.Enumerate ( EnumerateDomain )
import Conjure.Process.ValidateConstantForDomain ( validateConstantForDomain )
import Conjure.Representations ( downC )


translateParameter ::
    MonadFail m =>
    MonadLog m =>
    NameGen m =>
    EnumerateDomain m =>
    MonadIO m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Bool ->      -- Prepare input files for the Glasgow graph solver
    Model ->     -- eprime model
    Model ->     -- essence param
    m Model      -- eprime param

translateParameter graphSolver eprimeModel0 essenceParam0 = do
    logDebug $ "[eprimeModel  0]" <+-> pretty essenceParam0
    logDebug $ "[essenceParam 0]" <+-> pretty essenceParam0
    (eprimeModel, essenceParam1) <- removeEnumsFromParam eprimeModel0 essenceParam0
    logDebug $ "[eprimeModel  1]" <+-> pretty eprimeModel
    logDebug $ "[essenceParam 1]" <+-> pretty essenceParam1

    let eprimeLettingsForEnums =
            [ (nm, fromInt (genericLength vals))
            | nm1                                          <- eprimeModel |> mInfo |> miEnumGivens
            , Declaration (LettingDomainDefnEnum nm2 vals) <- essenceParam0 |> mStatements
            , nm1 == nm2
            , let nm = nm1 `mappend` "_EnumSize"
            ]

    (essenceParam, generatedLettingNames) <- finiteGivensParam eprimeModel essenceParam1 eprimeLettingsForEnums
    logDebug $ "[essenceParam 2]" <+-> pretty essenceParam

    let essenceLettings   = extractLettings essenceParam
    let essenceGivenNames = eprimeModel |> mInfo |> miGivens
    let essenceGivens     = eprimeModel |> mInfo |> miRepresentations
                                        |> filter (\ (n,_) -> n `elem` essenceGivenNames )

    logDebug $ "[essenceLettings  ]" <+-> vcat [ pretty n <> ":" <+> pretty x | (n,x) <- essenceLettings ]
    logDebug $ "[essenceGivenNames]" <+-> vcat (map pretty essenceGivenNames)
    logDebug $ "[essenceGivens    ]" <+-> vcat [ pretty n <> ":" <+> pretty x | (n,x) <- essenceGivens ]

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

    let allLettings = (eprimeModel |> mInfo |> miLettings)
                   ++ essenceLettings
                   ++ map (second Constant) eprimeLettingsForEnums

    essenceLettings' <- forM essenceLettings $ \ (name, val) -> do
        constant <- instantiateExpression allLettings val
        return (name, constant)
    logDebug $ "[essenceLettings' ]" <+> vcat [ pretty n <> ":" <+-> pretty x | (n,x) <- essenceLettings' ]

    essenceGivens' <- forM essenceGivens $ \ (name, dom) -> do
        constant <- instantiateDomain allLettings dom
        return (name, constant)
    logDebug $ "[essenceGivens'   ]" <+> vcat [ pretty n <> ":" <+-> pretty x | (n,x) <- essenceGivens' ]

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
                                    cTy2 <- typeOfDomain d
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
                                                             , "constant:" <+-> pretty c
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

    logDebug $ "[essenceGivensAndLettings']" <+-> vcat [ vcat [ "name    :" <+> pretty n
                                                             , "domain  :" <+> pretty d
                                                             , "constant:" <+-> pretty c
                                                             ]
                                                      | (n,d,c) <- essenceGivensAndLettings'
                                                      ]

    errs <- execWriterT $ forM_ essenceGivensAndLettings' $ \ (nm, dom, val) -> do
        mres <- runExceptT $ validateConstantForDomain nm val dom
        case mres of
            Left err -> tell [err]
            Right () -> return ()
    unless (null errs) (userErr errs)

    let
        decorateWithType p@(_, _, TypedConstant{}) = return p
        decorateWithType (name, domain, constant) | emptyCollection constant = do
            ty <- typeOfDomain domain
            return (name, domain, TypedConstant constant ty)
        decorateWithType p = return p

    when graphSolver $ do
        forM_ essenceGivensAndLettings' $ \ (n,d,c) -> do
            let pairs =
                    case d of
                        DomainFunction _ _ (DomainTuple [DomainInt{}, DomainInt{}]) _ ->
                            case c of
                                ConstantAbstract (AbsLitFunction rows) ->
                                    [ case row of
                                        (ConstantAbstract (AbsLitTuple [a, b]), _) -> [a,b]
                                        _ -> []
                                    | row <- rows ]
                                _ -> []
                        DomainRelation _ _ ([DomainInt{}, DomainInt{}, _]) ->
                            case c of
                                ConstantAbstract (AbsLitRelation rows) ->
                                    [ case row of
                                        [a, b, _] -> [a,b]
                                        _ -> []
                                    | row <- rows ]
                                _ -> []
                        _ -> []
            let csvLines = [ pretty a <> "," <> pretty b | [a,b] <- sortNub pairs ]
            unless (null pairs) $
                liftIO $ writeFile ("given-" ++ show (pretty n) ++ ".csv") (render 100000 (vcat csvLines))

        let essenceFindNames = eprimeModel |> mInfo |> miFinds
        let essenceFinds = eprimeModel |> mInfo |> miRepresentations |> filter (\ (n,_) -> n `elem` essenceFindNames )
        forM_ essenceFinds $ \ (n, d) -> do
            case d of
                DomainFunction _ _ (DomainInt _ [RangeBounded a b]) _ -> do
                        a' <- instantiateExpression allLettings a
                        b' <- instantiateExpression allLettings b
                        case (a', b') of
                            (ConstantInt _ a'', ConstantInt _ b'') -> do
                                let csvLines =
                                        [ pretty i <> "," <> name
                                        | i <- [a''..b'']
                                        , let name = pretty n <> "_Function1D_" <> pretty (padLeft 5 '0' (show i))
                                        ]
                                unless (null csvLines) $
                                    liftIO $ writeFile ("find-" ++ show (pretty n) ++ ".csv") (render 100000 (vcat csvLines))
                            _ -> userErr1 $ "Unsupported domain for --graph-solver:" <+> pretty d
                _ -> return ()


    eprimeLettings
        :: [(Name, Domain HasRepresentation Constant, Constant)]
        <- failToUserError $ concatMapM downC essenceGivensAndLettings' >>= mapM decorateWithType
    logDebug $ "[eprimeLettings           ]" <+> vcat [ vcat [ "name    :" <+> pretty n
                                                             , "domain  :" <+> pretty d
                                                             , "constant:" <+-> pretty c
                                                             ]
                                                      | (n,d,c) <- eprimeLettings
                                                      ]

    return $ languageEprime def
        { mStatements = transformBi (\ _ -> TagInt) $
            [ Declaration (Letting n (Constant x))
            | (n, _, x) <- eprimeLettings
            ] ++
            [ Declaration (Letting n (Constant x))
            | (n, x) <- eprimeLettingsForEnums
            ]
        }
