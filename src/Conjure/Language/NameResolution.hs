{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TupleSections #-}

module Conjure.Language.NameResolution
    ( resolveNames
    , resolveNamesX
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty


resolveNames :: (MonadLog m, MonadFail m) => Model -> m Model
resolveNames model = flip evalStateT (freshNames model, []) $ do
    statements <- forM (mStatements model) $ \ st ->
        case st of
            Declaration decl ->
                case decl of
                    FindOrGiven forg nm dom       -> do
                        dom' <- resolveD dom
                        modify (second ((nm, DeclNoRepr forg nm dom') :))
                        return (Declaration (FindOrGiven forg nm dom'))
                    Letting nm x                  -> do
                        x' <- resolveX x
                        modify (second ((nm, Alias x') :))
                        return (Declaration (Letting nm x'))
                    _ -> fail ("Unexpected declaration:" <+> pretty st)
            SearchOrder{} -> return st
            Where xs -> Where <$> mapM resolveX xs
            Objective obj x -> Objective obj <$> resolveX x
            SuchThat xs -> SuchThat <$> mapM resolveX xs
    mapM_ check (universeBi statements)
    duplicateNames <- gets (snd >>> map fst >>> histogram >>> filter (\ (_,n) -> n > 1 ) >>> map fst)
    unless (null duplicateNames) $
        userErr ("Some names are defined multiple times:" <+> prettyList id "," duplicateNames)
    return model { mStatements = statements }

shadowing
    :: ( MonadFail m
       , MonadState ([Name], [(Name, ReferenceTo)]) m
       )
    => Expression
    -> m Expression
shadowing p@(Comprehension _ is) = do
    let generators = concat
            [ names
            | Generator gen <- is
            , let pat = generatorPat gen
            , let names = [ n | n@Name{} <- universeBi pat ]
            ]
    ctxt <- gets snd
    let shadows = [ g | g <- generators, g `elem` map fst ctxt ]
    let nextName = do
            (names,ctxt') <- gets id
            modify $ const (tail names, ctxt')
            return (head names)
    shadowsNew <- forM shadows $ \ s -> do n <- nextName ; return (s,n)
    let f n = fromMaybe n (lookup n shadowsNew)
    return (transformBi f p)
shadowing p = return p


resolveNamesX :: MonadFail m => Expression -> m Expression
resolveNamesX x = do
    x' <- evalStateT (resolveX x) ([],[])
    mapM_ check (universe x')
    return x'


check :: MonadFail m => Expression -> m ()
check (Reference nm Nothing) = fail ("Undefined:" <+> pretty nm)
check _ = return ()


resolveX
    :: ( MonadFail m
       , MonadState ([Name], [(Name, ReferenceTo)]) m
       )
    => Expression
    -> m Expression

resolveX (Reference nm Nothing) = do
    ctxt <- gets snd
    mval <- gets (lookup nm . snd)
    case mval of
        Nothing -> fail $ vcat $ ("Undefined reference:" <+> pretty nm)
                               : ("Bindings in context:" : prettyContext ctxt)
        Just r  -> return (Reference nm (Just r))

resolveX p@(Reference nm Just{}) = do                   -- this is for re-resolving
    mval <- gets (lookup nm . snd)
    case mval of
        Nothing -> return p                             -- hence, do not fail if not in the context
        Just r  -> return (Reference nm (Just r))

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
                                    Single nm' -> DeclNoRepr Quantified nm' dom'
                                    _ -> InComprehension gen''
                                )
                        GenDomainHasRepr nm dom ->
                            return
                                ( GenDomainHasRepr nm dom
                                , DeclHasRepr Quantified nm dom
                                )
                        GenInExpr pat expr -> do
                            expr' <- resolveX expr
                            let gen'' = GenInExpr pat expr'
                            return ( gen'' , InComprehension gen'' )
                    forM_ (universeBi (generatorPat gen)) $ \ nm ->
                        modify (second ((nm, refto) :))
                    return (Generator gen')
                Filter y -> Filter <$> resolveX y
            x' <- resolveX x
            return (Comprehension x' is')
        _ -> bug "NameResolution.resolveX.shadowing"
resolveX x = descendM resolveX x


resolveD
    :: ( MonadFail m
       , MonadState ([Name], [(Name, ReferenceTo)]) m
       )
    => Domain () Expression
    -> m (Domain () Expression)
resolveD (DomainReference _ (Just d)) = resolveD d
resolveD (DomainReference nm Nothing) = do
    mval <- gets (lookup nm . snd)
    case mval of
        Nothing -> userErr ("Undefined reference to a domain:" <+> pretty nm)
        Just (Alias (Domain r)) -> resolveD r
        Just x -> userErr ("Expected a domain, but got an expression:" <+> pretty x)
resolveD d = do
    d' <- descendM resolveD d
    mapM resolveX d'

