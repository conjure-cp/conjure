{-# LANGUAGE ScopedTypeVariables #-}

module Conjure.UI.TranslateSolution
    ( translateSolution
    , prepareTranslateSolution
    ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Type ( TypeCheckerMode(..) )
import Conjure.Language.Constant ( normaliseConstant )
import Conjure.Language.Domain ( Domain, HasRepresentation )
import Conjure.Language.Pretty
import Conjure.Language.Instantiate
import Conjure.Process.Enums ( removeEnumsFromParam, addEnumsAndUnnamedsBack )
import Conjure.Process.Enumerate ( EnumerateDomain )
import Conjure.UI.TranslateParameter ( translateParameter )
import Conjure.Representations ( up )

-- text
import qualified Data.Text as T ( pack, stripPrefix )

-- unordered-containers
import qualified Data.HashMap.Strict as M

-- containers
import qualified Data.Set as S


data PreparedLetting = PreparedLetting
    { plName  :: Name
    , plExpr  :: Expression
    , plConst :: Maybe Constant
    }


translateSolution ::
    MonadFailDoc m =>
    MonadLog m =>
    NameGen m =>
    EnumerateDomain m =>
    MonadIO m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model ->      -- eprime model
    Model ->      -- essence param
    Model ->      -- eprime solution
    m Model       -- essence solution

translateSolution eprimeModel essenceParam eprimeSolution = do
    tr <- prepareTranslateSolution eprimeModel essenceParam
    tr eprimeSolution


prepareTranslateSolution ::
    forall m .
    MonadFailDoc m =>
    MonadLog m =>
    NameGen m =>
    EnumerateDomain m =>
    MonadIO m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model ->      -- eprime model
    Model ->      -- essence param
    m (Model -> m Model)

-- Precompute solution-invariant data so per-solution translation is cheaper.
prepareTranslateSolution (undoUnderscores -> eprimeModel) (undoUnderscores -> essenceParam') = do

    eprimeParam <- translateParameter False eprimeModel essenceParam'
    (_, essenceParam) <- removeEnumsFromParam eprimeModel essenceParam'

    let eprimeLettingsForEnums =
            [ (nm, fromInt (genericLength vals))
            | nm1                                          <- eprimeModel |> mInfo |> miEnumGivens
            , Declaration (LettingDomainDefnEnum nm2 vals) <- essenceParam' |> mStatements
            , nm1 == nm2
            , let nm = nm1 `mappend` "_EnumSize"
            ]

    let essenceFindNames = eprimeModel |> mInfo |> miFinds
    let essenceFinds     = eprimeModel |> mInfo |> miRepresentations
                                       |> filter (\ (n,_) -> n `elem` essenceFindNames )

    let normalizeLetting (name, val) = (name, maybe val Constant (e2c val))

    let prefixLettings0 =
            map normalizeLetting (extractLettings essenceParam ++ extractLettings eprimeParam)
    let suffixLettings0 =
            map normalizeLetting $
                extractLettings eprimeModel
                ++ (eprimeModel |> mInfo |> miLettings)
                ++ eprimeLettingsForEnums

    let fixedLettings0 = prefixLettings0 ++ suffixLettings0

    let exprNames :: Expression -> S.Set Name
        exprNames expr = S.fromList (universeBi expr :: [Name])

    let dependentNames =
            let
                findNames = S.fromList (eprimeModel |> mInfo |> miFinds)
                step deps =
                    S.union deps $ S.fromList
                        [ nm
                        | (nm, expr) <- fixedLettings0
                        , not (S.null (exprNames expr `S.intersection` deps))
                        ]
                go deps =
                    let deps' = step deps
                    in  if deps' == deps then deps else go deps'
            in
                go findNames

    let fixedContextExpr = fixedLettings0

    let prepareLettings :: [(Name, Expression)] -> m [PreparedLetting]
        prepareLettings = mapM $ \ (name, expr) -> do
            if name `S.member` dependentNames
                then return (PreparedLetting name expr Nothing)
                else do
                    c <- case expr of
                        Constant c -> return c
                        _          -> instantiateExpression fixedContextExpr expr
                    return (PreparedLetting name (Constant c) (Just c))

    prefixPrepared <- prepareLettings prefixLettings0
    suffixPrepared <- prepareLettings suffixLettings0

    let domainNames :: Domain HasRepresentation Expression -> S.Set Name
        domainNames dom = S.fromList (universeBi dom :: [Name])

    essenceFindsPrepared <- forM essenceFinds $ \ (name, dom) -> do
        if not (S.null (domainNames dom `S.intersection` dependentNames))
            then return (name, dom, Nothing)
            else do
                constant <- instantiateDomain fixedContextExpr dom
                return (name, dom, Just constant)

    let
        intToEnumConstant :: M.HashMap (Integer, Name) Constant
        intToEnumConstant = M.fromList $ concat
            [ [ ((i,ename), ConstantEnum ename vals v)
              | (i,v) <- zip allNats vals
              ]
            | Declaration (LettingDomainDefnEnum ename vals)
                    <- mStatements essenceParam'
                    ++ eprimeModel |> mInfo |> miEnumLettings |> map Declaration
            ]

    let
        unnameds :: [(Name, Expression)]
        unnameds = eprimeModel |> mInfo |> miUnnameds

    let (unnamedsStatic, unnamedsDynamic) =
            partition (\ (_, expr) -> S.null (exprNames expr `S.intersection` dependentNames)) unnameds

    unnamedsStaticDecls <- forM unnamedsStatic $ \ (n, s') -> do
        s <- instantiateExpression fixedContextExpr (maybe s' Constant (e2c s'))
        case s of
            ConstantInt _ size -> return $
                let nms = [ mconcat [n, "_", Name (T.pack (show i))]
                          | i <- [1 .. size]
                          ]
                in  Declaration (LettingDomainDefnEnum n nms)
            _ -> failDoc $ vcat [ "Expecting an integer value for" <+> pretty n
                             , "But got:" <+> pretty s
                             ]

    let origDomainMap = M.fromList (eprimeModel |> mInfo |> miOriginalDomains)

    let
        mkUnnamedsDecls ::
            MonadFailDoc m =>
            EnumerateDomain m =>
            NameGen m =>
            (?typeCheckerMode :: TypeCheckerMode) =>
            [(Name, Expression)] ->
            m [Statement]
        mkUnnamedsDecls ctxtExpr = forM unnamedsDynamic $ \ (n, s') -> do
            s <- instantiateExpression ctxtExpr (maybe s' Constant (e2c s'))
            case s of
                ConstantInt _ size -> return $
                    let nms = [ mconcat [n, "_", Name (T.pack (show i))]
                              | i <- [1 .. size]
                              ]
                    in  Declaration (LettingDomainDefnEnum n nms)
                _ -> failDoc $ vcat [ "Expecting an integer value for" <+> pretty n
                                 , "But got:" <+> pretty s
                                 ]

    let
        evalLetting ::
            MonadFailDoc m =>
            EnumerateDomain m =>
            NameGen m =>
            (?typeCheckerMode :: TypeCheckerMode) =>
            [(Name, Expression)] -> (Name, Expression) -> m (Name, Constant)
        evalLetting ctxt (name, expr) =
            case expr of
                Constant c -> return (name, c)
                _ -> do
                    c <- instantiateExpression ctxt expr
                    return (name, c)

    let
        translateOne ::
            MonadFailDoc m =>
            MonadLog m =>
            NameGen m =>
            EnumerateDomain m =>
            MonadIO m =>
            (?typeCheckerMode :: TypeCheckerMode) =>
            Model -> m Model
        translateOne (undoUnderscores -> eprimeSolution) = do
            let solutionLettings0 = map normalizeLetting (extractLettings eprimeSolution)
            let prefixExpr = [ (plName p, plExpr p) | p <- prefixPrepared ]
            let suffixExpr = [ (plName p, plExpr p) | p <- suffixPrepared ]
            let contextExpr = prefixExpr ++ solutionLettings0 ++ suffixExpr

            prefixConsts <- forM prefixPrepared $ \ p ->
                case plConst p of
                    Just c  -> return (plName p, c)
                    Nothing -> evalLetting contextExpr (plName p, plExpr p)
            solutionConsts <- forM solutionLettings0 (evalLetting contextExpr)
            suffixConsts <- forM suffixPrepared $ \ p ->
                case plConst p of
                    Just c  -> return (plName p, c)
                    Nothing -> evalLetting contextExpr (plName p, plExpr p)

            let eprimeLettings' = prefixConsts ++ solutionConsts ++ suffixConsts

            essenceFinds' <- forM essenceFindsPrepared $ \ (name, dom, domConst) -> do
                constant <- case domConst of
                    Just c  -> return c
                    Nothing -> instantiateDomain contextExpr dom
                return (name, constant)

            essenceLettings <- forM essenceFinds' $ \ (name, domain) -> do
                (_, constant) <- up eprimeLettings' (name, domain)
                let origDomain = fromMaybe (bug ("Missing original domain for:" <+> pretty name))
                                 (M.lookup name origDomainMap)
                return (name, origDomain, constant)

            unnamedsDynamicDecls <- mkUnnamedsDecls contextExpr

            let outStmts =
                    unnamedsStaticDecls ++ unnamedsDynamicDecls ++
                    sortNub
                        [ Declaration (Letting n (Constant (normaliseConstant y)))
                        | (n, d, x) <- essenceLettings
                        , let y = addEnumsAndUnnamedsBack
                                        (map fst unnameds)
                                        intToEnumConstant
                                        d x
                        ]

            let undefs = [ u | u@ConstantUndefined{} <- universeBi outStmts ]

            if null undefs
                then return def { mStatements = outStmts }
                else bug $ vcat
                    [ "Undefined values in the output:" <++> vcat (map pretty undefs)
                    , ""
                    , "Complete output would have been the following."
                    , ""
                    , pretty $ def { mStatements = outStmts }
                    ]

    return translateOne

undoUnderscores :: Model -> Model
undoUnderscores model =
    let
        -- SR doesn't support identifiers that start with _
        -- we replaced them with UNDERSCORE__ in prologue
        -- undo that here
        onName :: Name -> Name
        onName (Name t) =
            case T.stripPrefix "UNDERSCORE__" t of
                Nothing -> Name t
                Just t' -> Name (mappend "_" t')
        onName n = n

    in
        transformBi onName model
