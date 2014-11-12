{-# LANGUAGE TupleSections #-}

module Conjure.Process.Enums
    ( deenumifyModel
    , deenumifyParam
    , enumify
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty


-- | The argument is a model before nameResolution.
--   Only intended to work on problem specifications.
deenumifyModel :: (MonadFail m, MonadLog m) => Model -> m Model
deenumifyModel = deenumifyModel_LettingEnums >=> deenumifyModel_GivenEnums

    where

        deenumifyModel_LettingEnums model = do
            (statements', (enumDomainNames, nameToIntMapping)) <-
                flip runStateT ([], []) $ forM (mStatements model) $ \ st ->
                    case st of
                        Declaration (LettingDomainDefnEnum ename names) -> do
                            namesBefore <- gets (map fst . snd)
                            let outDomain = DomainInt [RangeBounded (fromInt 1) (fromInt (length names))]
                            if null (names `intersect` namesBefore)
                                then modify ( ( [(ename, outDomain)]
                                              , zip names allNats
                                              ) `mappend` )
                                else fail $ vcat [ "Some of the members of this enum domain seem to be defined"
                                                 , "as part of other enum domains."
                                                 , "While working on domain:" <+> pretty st
                                                 ]
                            return (Declaration (Letting ename (Domain outDomain)))
                        _ -> return st

            let onX (Reference nm Nothing)
                    | Just i <- lookup nm nameToIntMapping
                    = fromInt i
                onX p = p;

            let
                nameToX :: Name -> Expression
                nameToX nm = case lookup nm nameToIntMapping of
                    Nothing -> bug (pretty nm <+> "is used in a domain, but it isn't a member of the enum domain.")
                    Just i  -> fromInt i

            let
                onD :: Domain () Expression -> Domain () Expression
                onD (DomainEnum nm (Just ranges))
                    | Just _ <- lookup nm enumDomainNames
                    = DomainInt (map (fmap nameToX) ranges)
                onD (DomainEnum nm Nothing)
                    | Just d <- lookup nm enumDomainNames
                    = DomainReference nm (Just d)
                onD (DomainReference nm Nothing)
                    | Just d <- lookup nm enumDomainNames
                    = DomainReference nm (Just d)
                onD p = p

            let model' = model { mStatements = statements' 
                                    |> transformBi onX
                                    |> transformBi onD
                                }

            return model'

        deenumifyModel_GivenEnums model = do
            (statements', enumDomainNames) <-
                flip runStateT [] $ forM (mStatements model) $ \ st ->
                    case st of
                        Declaration (GivenDomainDefnEnum name) -> do
                            let nameS      = name `mappend` "_EnumSize"
                            let outDomainS = DomainInt []
                            let outDomain  = DomainInt [RangeBounded (fromInt 1)
                                                (Reference nameS (Just (Alias (Domain outDomainS))))]
                            modify ([(name, outDomain)] `mappend`)
                            return [ Declaration (FindOrGiven Given nameS outDomainS) ]
                        _ -> return [st]

            let
                onD :: Domain () Expression -> Domain () Expression
                onD (DomainEnum      nm Nothing)
                    | Just d <- lookup nm enumDomainNames
                    = DomainReference nm (Just d)
                onD (DomainReference nm Nothing)
                    | Just d <- lookup nm enumDomainNames
                    = DomainReference nm (Just d)
                onD p = p

            let model' = model { mStatements = concat statements' }
                       |> transformBi onD

            logDebug $ "Recording enumGivens:" <+> prettyList id "," (map fst enumDomainNames)

            return model'

deenumifyParam
    :: (MonadFail m, MonadLog m)
    => Model -> Model -> m Model
deenumifyParam model param = do
    let allStatements = map (False,) (map Declaration (miEnumLettings (mInfo model)))
                     ++ map (True,)  (mStatements param)

    (statements', (enumDomainNames, nameToIntMapping)) <-
        flip runStateT ([], []) $ forM allStatements $ \ (keep,st) ->
            case st of
                Declaration (LettingDomainDefnEnum ename names) -> do
                    namesBefore <- gets (map fst . snd)
                    let outDomain = DomainInt [RangeBounded (fromInt 1) (fromInt (length names))]
                    if null (names `intersect` namesBefore)
                        then modify ( ( [(ename, outDomain)]
                                      , zip names allNats
                                      ) `mappend` )
                        else fail $ vcat [ "Some of the members of this enum domain seem to be defined"
                                         , "as part of other enum domains."
                                         , "While working on domain:" <+> pretty st
                                         ]
                    return (Just (Declaration (Letting ename (Domain outDomain))))
                _ -> return (if keep then Just st else Nothing)

    let onX (Reference nm Nothing)
            | Just i <- lookup nm nameToIntMapping
            = fromInt i
        onX p = p;

    let
        nameToX :: Name -> Expression
        nameToX nm = case lookup nm nameToIntMapping of
            Nothing -> bug (pretty nm <+> "is used in a domain, but it isn't a member of the enum domain.")
            Just i  -> fromInt i

    let
        onD :: Domain () Expression -> Domain () Expression
        onD (DomainEnum nm (Just ranges))
            | Just _ <- lookup nm enumDomainNames
            = DomainInt (map (fmap nameToX) ranges)
        onD (DomainEnum nm Nothing)
            | Just d <- lookup nm enumDomainNames
            = DomainReference nm (Just d)
        onD (DomainReference nm Nothing)
            | Just d <- lookup nm enumDomainNames
            = DomainReference nm (Just d)
        onD p = p

    let param' = param { mStatements = catMaybes statements'
                            |> transformBi onX
                            |> transformBi onD
                       }
    return param'

-- | Using the original domains from the Essence file.
--   Converting integers back to enum constants.
-- TODO: complete enumify
enumify
    :: [((Int, Name), Constant)]
    -> Domain () Expression
    -> Constant
    -> Constant

enumify ctxt domain constant = case (domain, constant) of

    (DomainBool , c) -> c
    (DomainInt{}, c) -> c

    (DomainEnum      ename _, ConstantInt i) ->
        fromMaybe (bug $ "enumify:" <+> pretty (i, ename))
                  (lookup (i, ename) ctxt)

    (DomainReference ename _, ConstantInt i) ->
        fromMaybe (bug $ "enumify:" <+> pretty (i, ename))
                  (lookup (i, ename) ctxt)

    (DomainTuple ds, ConstantTuple cs) ->
        ConstantTuple [ enumify ctxt d c
                      | (d,c) <- zip ds cs ]

    (DomainMatrix _ inner, ConstantMatrix index vals) ->
        ConstantMatrix index (map (enumify ctxt inner) vals)

    (DomainSet _ _ inner, ConstantSet vals) ->
        ConstantSet (map (enumify ctxt inner) vals)

    (DomainMSet _ _ inner, ConstantMSet vals) ->
        ConstantMSet (map (enumify ctxt inner) vals)

    (DomainFunction _ _ fr to, ConstantFunction vals) ->
        ConstantFunction [ (enumify ctxt fr a, enumify ctxt to b)
                         | (a,b) <- vals ]

    (DomainRelation _ _ inners, ConstantRelation vals) ->
        ConstantRelation [ [ enumify ctxt d c | (d,c) <- zip inners line ]
                         | line <- vals ]

    _ -> bug ("enumify:" <+> pretty (show domain))

