{-# LANGUAGE TupleSections #-}

module Conjure.Language.NameResolution
    ( resolveNames
    , resolveNamesMulti
    , resolveNamesX
    , resolveX, resolveD -- actually internal, use with care
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.TypeOf
import Conjure.Language.Pretty


resolveNamesMulti :: (MonadLog m, MonadFail m, MonadUserError m, NameGen m) => [Model] -> m [Model]
resolveNamesMulti = flip evalStateT [] . go
    where
        go [] = return []
        go (m:ms) = (:) <$> resolveNames_ m <*> go ms

resolveNames :: (MonadLog m, MonadFail m, MonadUserError m, NameGen m) => Model -> m Model
resolveNames = flip evalStateT [] . resolveNames_

resolveNames_
    :: ( MonadFail m
       , MonadUserError m
       , MonadState [(Name, ReferenceTo)] m
       , NameGen m
       )
    => Model -> m Model
resolveNames_ model = do
    statements <- mapM resolveStatement (mStatements model)
    mapM_ check (universeBi statements)
    return model { mStatements = statements }

-- this is for when a name will shadow an already existing name that is outside of this expression
-- we rename the new names to avoid name shadowing
shadowing
    :: ( MonadFail m
       , MonadState [(Name, ReferenceTo)] m
       , NameGen m
       )
    => Expression
    -> m Expression
shadowing p@(Comprehension _ is) = do
    -- list of names originating from this comprehension
    let generators = concat
            [ names
            | Generator gen <- is
            , let pat = generatorPat gen
            , let names = [ n | n@Name{} <- universeBi pat ]
            ]
    ctxt <- gets id
    -- a subset of names originating from this comprehension that will shadow already existing names
    let shadows = [ g | g <- generators, g `elem` map fst ctxt ]
    shadowsNew <- forM shadows $ \ s -> do n <- nextName "shadow" ; return (s,n)
    let f n = fromMaybe n (lookup n shadowsNew)
    return (transformBi f p)
shadowing p = return p


resolveNamesX :: (MonadFail m, MonadUserError m, NameGen m) => Expression -> m Expression
resolveNamesX x = do
    x' <- evalStateT (resolveX x) []
    mapM_ check (universe x')
    return x'


check :: MonadFail m => Expression -> m ()
check (Reference nm Nothing) = fail ("Undefined:" <+> pretty nm)
check _ = return ()


resolveStatement
    :: ( MonadFail m
       , MonadUserError m
       , MonadState [(Name, ReferenceTo)] m
       , NameGen m
       )
    => Statement
    -> m Statement

resolveStatement st =
    case st of
        Declaration decl ->
            case decl of
                FindOrGiven forg nm dom       -> do
                    dom' <- resolveD dom
                    modify ((nm, DeclNoRepr forg nm dom' NoRegion) :)
                    return (Declaration (FindOrGiven forg nm dom'))
                Letting nm x                  -> do
                    x' <- resolveX x
                    modify ((nm, Alias x') :)
                    return (Declaration (Letting nm x'))
                LettingDomainDefnUnnamed nm x -> do
                    x' <- resolveX x
                    modify ((nm, Alias (Domain (DomainUnnamed nm x'))) :)
                    return (Declaration (LettingDomainDefnUnnamed nm x'))
                LettingDomainDefnEnum _ nms -> do
                    modify ( [ (nm, Alias (Constant (ConstantInt (Just nm) i)))
                             | (nm, i) <- zip nms [1..]
                             ] ++)
                    return st
                GivenDomainDefnEnum{}       -> return st             -- ignoring
        SearchOrder xs -> SearchOrder <$> mapM resolveSearchOrder xs
        SearchHeuristic nm -> do
            let allowed = ["static", "sdf", "conflict", "srf", "ldf", "wdeg", "domoverwdeg"]
            if nm `elem` allowed
                then return (SearchHeuristic nm)
                else userErr1 $ vcat [ "Invalid heuristic:" <+> pretty nm
                                     , "Allowed values are:" <+> prettyList id "," allowed
                                     ]
        Where xs -> Where <$> mapM resolveX xs
        Objective obj x -> Objective obj <$> resolveX x
        SuchThat xs -> SuchThat <$> mapM resolveX xs


resolveSearchOrder
    :: ( MonadFail m
       , MonadUserError m
       , MonadState [(Name, ReferenceTo)] m
       , NameGen m
       )
    => SearchOrder
    -> m SearchOrder
resolveSearchOrder (BranchingOn nm) = do
    ctxt <- gets id
    mval <- gets (lookup nm)
    case mval of
        Nothing -> userErr1 $ vcat $ ("Undefined reference:" <+> pretty nm)
                                   : ("Bindings in context:" : prettyContext ctxt)
        Just{}  -> return (BranchingOn nm)
resolveSearchOrder (Cut x) =
    let f Find = CutFind
        f forg = forg
    in  Cut . transformBi f <$> resolveX x


resolveX
    :: ( MonadFail m
       , MonadUserError m
       , MonadState [(Name, ReferenceTo)] m
       , NameGen m
       )
    => Expression
    -> m Expression

resolveX (Reference nm Nothing) = do
    ctxt <- gets id
    mval <- gets (lookup nm)
    case mval of
        Nothing -> userErr1 $ vcat $ ("Undefined reference:" <+> pretty nm)
                                   : ("Bindings in context:" : prettyContext ctxt)
        Just r  -> return (Reference nm (Just r))

resolveX p@(Reference nm (Just refto)) = do             -- this is for re-resolving
    mval <- gets (lookup nm)
    case mval of
        Nothing -> return p                             -- hence, do not fail if not in the context
        Just DeclNoRepr{}                               -- if the newly found guy doesn't have a repr
            | DeclHasRepr{} <- refto                    -- but the old one did, do not update
            -> return p
        Just (DeclNoRepr forg_ nm_ dom_ _)              -- if the newly found guy doesn't have a repr
            | DeclNoRepr _ _ _ region <- refto          -- and the old one didn't have one either
                                                        -- preserve the region information
            -> return (Reference nm (Just (DeclNoRepr forg_ nm_ dom_ region)))
        Just (Alias r) -> do
            r' <- resolveX r
            return (Reference nm (Just (Alias r')))
        Just r ->
            return (Reference nm (Just r))

resolveX (AbstractLiteral lit) = AbstractLiteral <$> resolveAbsLit lit

resolveX (Domain x) = Domain <$> resolveD x

resolveX p@Comprehension{} = scope $ do
    p' <- shadowing p
    case p' of
        Comprehension x is -> do
            is' <- forM is $ \ i -> case i of
                Generator gen -> do
                    (gen', refto) <- case gen of
                        GenDomainNoRepr pat dom -> do
                            dom' <- resolveD dom
                            let gen'' = GenDomainNoRepr pat dom'
                            return
                                ( gen''
                                , case pat of
                                    Single nm' -> DeclNoRepr Quantified nm' dom' NoRegion
                                    _ -> InComprehension gen''
                                )
                        GenDomainHasRepr nm dom -> do
                            dom' <- resolveD dom
                            return
                                ( GenDomainHasRepr nm dom'
                                , DeclHasRepr Quantified nm dom'
                                )
                        GenInExpr pat expr -> do
                            expr' <- resolveX expr
                            let gen'' = GenInExpr pat expr'
                            return ( gen'' , InComprehension gen'' )
                    forM_ (universeBi (generatorPat gen)) $ \ nm ->
                        modify ((nm, refto) :)
                    return (Generator gen')
                Condition y -> Condition <$> resolveX y
                ComprehensionLetting nm expr -> do
                    expr' <- resolveX expr
                    modify ((nm, Alias expr') :)
                    return (ComprehensionLetting nm expr')
            x' <- resolveX x
            return (Comprehension x' is')
        _ -> bug "NameResolution.resolveX.shadowing"

resolveX (WithLocals body (AuxiliaryVars locals)) = scope $ do
    locals' <- mapM resolveStatement locals
    body'   <- resolveX body
    return (WithLocals body' (AuxiliaryVars locals'))

resolveX (WithLocals body (DefinednessConstraints locals)) = scope $ do
    locals' <- mapM resolveX locals
    body'   <- resolveX body
    return (WithLocals body' (DefinednessConstraints locals'))

resolveX x = descendM resolveX x


resolveD
    :: ( MonadFail m
       , MonadUserError m
       , MonadState [(Name, ReferenceTo)] m
       , NameGen m
       , Data r
       , Pretty r
       , Default r
       )
    => Domain r Expression
    -> m (Domain r Expression)
resolveD (DomainReference _ (Just d)) = resolveD d
resolveD (DomainReference nm Nothing) = do
    mval <- gets (lookup nm)
    case mval of
        Nothing -> userErr1 ("Undefined reference to a domain:" <+> pretty nm)
        Just (Alias (Domain r)) -> resolveD (changeRepr def r)
        Just x -> userErr1 ("Expected a domain, but got an expression:" <+> pretty x)
resolveD (DomainRecord ds) = fmap DomainRecord $ forM ds $ \ (n, d) -> do
    d' <- resolveD d
    t  <- typeOf d'
    modify ((n, RecordField n t) :)
    return (n, d')
resolveD (DomainVariant ds) = fmap DomainVariant $ forM ds $ \ (n, d) -> do
    d' <- resolveD d
    t  <- typeOf d'
    modify ((n, VariantField n t) :)
    return (n, d')
resolveD d = do
    d' <- descendM resolveD d
    mapM resolveX d'


resolveAbsLit
    :: ( MonadFail m
       , MonadUserError m
       , MonadState [(Name, ReferenceTo)] m
       , NameGen m
       )
    => AbstractLiteral Expression
    -> m (AbstractLiteral Expression)
resolveAbsLit (AbsLitVariant Nothing n x) = do
    x'   <- resolveX x
    mval <- gets id
    let
        isTheVariant (Alias (Domain d@(DomainVariant nms))) | Just{} <- lookup n nms = Just d
        isTheVariant _ = Nothing
    case mapMaybe (isTheVariant . snd) mval of
        (DomainVariant dom:_) -> return (AbsLitVariant (Just dom) n x')
        _ -> return (AbsLitVariant Nothing n x')
resolveAbsLit lit = (descendBiM resolveX >=> descendBiM resolveD') lit
    where
        resolveD' d = resolveD (d :: Domain () Expression)
