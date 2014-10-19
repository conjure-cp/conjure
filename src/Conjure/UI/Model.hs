{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE Rank2Types #-}

module Conjure.UI.Model
    ( outputOneModel
    , interactive, pickFirst
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Ops hiding ( opOr, opAnd, opIn, opEq, opLt, opMapOverDomain )
import Conjure.Language.Lenses
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.TypeOf
import Conjure.Language.DomainOf
import Conjure.Language.ModelStats ( givens, finds, declarations, lettings )
import Conjure.CState
import Conjure.Representations

import Data.Generics.Uniplate.Zipper ( zipperBi, fromZipper, hole, replaceHole )
import Data.Generics.Uniplate.Data ( uniplate, biplate, rewriteBiM )
import Data.Generics.Str ( Str )
import qualified Data.Text as T



type Question = (Expression, [Answer])
type Answer = (Expression, Model)

remaining :: (Functor m, MonadIO m) => Model -> m [Question]
remaining model = do
    let modelZipper = fromJustNote "Creating the initial zipper." (zipperBi model)
    fmap catMaybes $ forM (allContexts modelZipper) $ \ x -> do
        ys <- applicableRules (hole x)
        return $ if null ys
            then Nothing
            else Just ( hole x
                      , [ (y, fromZipper (replaceHole y x))
                        | y <- ys
                        ]
                      )



outputOneModel
    :: (forall a m . (MonadIO m, Pretty a) => Doc -> [a] -> m (Int, a))
    -> (String -> IO ())
    -> FilePath -> Int -> Model -> IO ()
outputOneModel driver printer dir i essence = do
    qs <- remaining essence
    forM_ (zip allNats qs) $ \ (nQ,(q,as)) -> do
        print ("Question" <+> pretty nQ <> ":" <+> pretty q)
        forM_ (zip allNats as) $ \ (nA,a) -> do
            print (nest 4 $ "Answer" <+> pretty nA <> ":" <+> pretty (fst a))
            print (nest 8 $ pretty (snd a))

    createDirectoryIfMissing True dir
    eprime <- nextModel driver printer (addTrueConstraints essence)
    let filename = dir </> "model" ++ show i ++ ".eprime"
    writeFile filename (renderWide eprime)
    printer $ show $ vcat
        [ pretty sel <+> "out of" <+> pretty (show opts) <+> "~~" <+> vcat (map pretty txts)
        | Decision txts opts sel <- miTrail (mInfo eprime)
        ]

pickFirst :: Monad m => Doc -> [a] -> m (Int, a)
pickFirst _question options =
    return (1, options `at` 0)

interactive :: (MonadIO m, Pretty a) => Doc -> [a] -> m (Int, a)
interactive question options = do
    liftIO $ print $ vcat
        [ question
        , nest 4 $ "Options:" <+>
            vcat [ nest 4 (pretty i <> ":" <+> pretty o)
                 | i <- allNats
                 | o <- options
                 ]
        ]
    ln <- liftIO getLine
    case readMay ln of
        Nothing -> userErr "You've got to enter an integer."
        Just n -> return (n, options `at` (n - 1))


-- | given an initial essence model,
--   and a list of ModelInfo's describing previously generated models,
--   generate the next model
--   or return Nothing if no other model can be generated
nextModel
    :: (forall a m . (MonadIO m, Pretty a) => Doc -> [a] -> m (Int, a))
    -> (String -> IO ())
    -> Model -> IO Model
nextModel askTheDriver printer initialEssence = do

    let lets  = lettings initialEssence
    let decls = declarations initialEssence

    let
        reportNode :: (MonadState CState m, MonadIO m) => Expression -> m ()
        reportNode x = do
            gets stNbExpression >>= \ nb -> liftIO $ printer $ show $ "--" <+> pretty nb <> ":" <+> pretty (show x)
            modify $ \ st -> st { stNbExpression = 1 + stNbExpression st }

    -- let
    --     f :: (MonadState CState m, MonadIO m) => Expression -> m Expression
    --     f (Reference nm Nothing) =
    --         case lookup nm decls of
    --             Nothing ->
    --                 case lookup nm lets of
    --                     Nothing -> do
    --                         liftIO $ putStrLn $ "what's this a reference to? " ++ show nm
    --                         return (Reference nm Nothing)
    --                     Just _  -> return (Reference nm Nothing)
    --             Just inpDom -> do
    --                 ascendants <- reportAscendants
    --
    --                 let domOpts = reprOptions inpDom
    --                 let numOptions = [1 .. length domOpts]
    --
    --                 when (null domOpts) $
    --                     bug $ "No representation matches this beast:" <++> pretty inpDom
    --
    --                 case numOptions of
    --                     [] -> do
    --                         liftIO $ putStrLn "exhausted=true"
    --                         return (Reference nm Nothing)
    --                     _ -> do
    --                         let question = vcat ( ("Selecting representation for:" <+> pretty nm)
    --                                             : map (nest 4) ascendants )
    --                         (numSelected, domSelected) <- askTheDriver question domOpts
    --                         let descr = vcat
    --                                 [ question
    --                                 , nest 4 $ "Options:" <+>
    --                                     vcat [ nest 4 (pretty i <> ":" <+> pretty o)
    --                                          | i <- allNats
    --                                          | o <- domOpts
    --                                          ]
    --                                 , nest 4 $ "Selected:"   <+> pretty domSelected
    --                                 , nest 4 $ "# Options: " <+> pretty (show numOptions)
    --                                 , nest 4 $ "# Selected:" <+> pretty numSelected
    --                                 ]
    --                         modify $ addReprToSt nm domSelected
    --                         modify $ addDecisionToSt descr numOptions numSelected
    --                         liftIO $ printer $ show descr
    --                         return (Reference nm (Just domSelected))
    --     f x = return x

    let initInfo = def { miGivens = map fst (givens initialEssence)
                       , miFinds  = map fst (finds  initialEssence)
                       }
    let pipeline =  -- tr (\ x -> do reportNode x; f x )
                    return
                >=> (rewriteBiM $ firstOfRules [ rule_TrueIsNoOp
                                               , rule_ToIntIsNoOp
                                               , rule_InlineFilterInsideMap
                                               , rule_TupleIndex
                                               , rule_SetIn_Explicit
                                               , rule_SetIn_Occurrence
                                               , rule_SetIn_ExplicitVarSizeWithMarker
                                               , rule_SetIn_ExplicitVarSizeWithFlags
                                               ])
                >=> updateDeclarations
    (statements', st) <- runStateT (pipeline (mStatements initialEssence))
                                   (def { stCurrInfo = initInfo
                                        })

    let model = initialEssence { mStatements = statements'
                               , mInfo = stCurrInfo st
                               }
    return (oneSuchThat model)


class ExpressionContainer a where
    tr :: MonadState CState m => (Expression -> m Expression) -> a -> m a

instance ExpressionContainer Statement where
    tr f x = do
        modify $ \ st -> st { stAscendants = Right x : stAscendants st }
        let (current, generate) = biplate x
        x' <- liftM generate $ mapM (tr f) (current :: Str Expression)
        modify $ \ st -> st { stAscendants = drop 1 (stAscendants st) }
        return x'

instance ExpressionContainer Expression where
    tr f x = do
        modify $ \ st -> st { stAscendants = Left x : stAscendants st }
        let (current, generate) = uniplate x
        x' <- liftM generate $ mapM (tr f) current
        modify $ \ st -> st { stAscendants = drop 1 (stAscendants st) }
        f x'

instance ExpressionContainer [Statement] where
    tr f = mapM (tr f)


addReprToSt :: Name -> Domain HasRepresentation Expression -> CState -> CState
addReprToSt nm dom st = st { stCurrInfo = addToInfo (stCurrInfo st)
                           , stAllReprs = nub $ (nm, dom) : inners ++ stAllReprs st
                           }
    where
        addToInfo i = i { miRepresentations = nub $ (nm, dom) : miRepresentations i }
        inners = case mkInners (nm,dom) of
            Left err -> bug err
            Right res -> res
        mkInners p = do
            mmids <- downD1 p
            case mmids of
                Nothing -> return []
                Just mids -> do
                    lows <- mapM mkInners mids
                    return (concat (mids:lows))
            
addDecisionToSt :: Doc -> [Int] -> Int -> CState -> CState
addDecisionToSt doc opts selected st =
    st { stCurrInfo = addToInfo (stCurrInfo st)
       }
    where addToInfo i = i { miTrail = miTrail i ++ [dec] }
          dec = Decision (doc |> renderWide |> stringToText |> T.lines) opts selected

reportAscendants :: MonadState CState m => m [Doc]
reportAscendants = do
    contexts <- gets stAscendants
    return
        [ "Context #" <> pretty i <> ":" <+> either pretty pretty c
        | (i,c) <- zip allNats contexts
        ]


-- | For every parameter and decision variable add a true-constraint.
--   A true-constraint has no effect, other than forcing Conjure to produce a representation.
--   It can be used to make sure that the decision variable doesn't get lost when it isn't mentioned anywhere.
--   It can also be used to produce "extra" representations.
--   Currently this function will add a true for every declaration, no matter if it is mentioned or not.
addTrueConstraints :: Model -> Model
addTrueConstraints m =
    let
        declarationNames = map fst (declarations m)
        mkTrueConstraint nm = Op $ MkOpTrue $ OpTrue [Reference nm Nothing]
        trueConstraints = map mkTrueConstraint declarationNames
    in
        m { mStatements = mStatements m ++ [SuchThat trueConstraints] }


oneSuchThat :: Model -> Model
oneSuchThat m = m { mStatements = others ++ [SuchThat suchThat] }
    where collect (SuchThat s) = ([], s)
          collect s = ([s], [])
          (others, suchThats) = mStatements m
                |> map collect
                |> mconcat
                |> second (filter (/= Constant (ConstantBool True)))
                |> second nub
          suchThat = if null suchThats
                      then [Constant (ConstantBool True)]
                      else suchThats


updateDeclarations :: (Functor m, MonadState CState m) => [Statement] -> m [Statement]
updateDeclarations statements = do
    reprs <- gets stAllReprs
    flip concatMapM statements $ \ st ->
        case st of
            Declaration (FindOrGiven h nm _) ->
                case [ d | (n,d) <- reprs, n == nm ] of
                    [] -> bug $ "No representation chosen for: " <+> pretty nm
                    domains -> flip concatMapM domains $ \ domain -> do
                        mouts <- runExceptT $ downD (nm, domain)
                        case mouts of
                            Left err -> bug err
                            Right outs -> return [Declaration (FindOrGiven h n (forgetRepr d)) | (n,d) <- outs]
            _ -> return [st]


representationOf :: MonadFail m => Expression -> m Name
representationOf (Reference _ Nothing) = fail "doesn't seem to have a representation"
representationOf (Reference _ (Just refTo)) =
    case refTo of
        DeclHasRepr _ _ d ->
            case reprAtTopLevel d of
                Nothing -> fail "doesn't seem to have a representation"
                Just NoRepresentation -> fail "doesn't seem to have a representation"
                Just (HasRepresentation r) -> return r
        _ -> fail "not a DeclHasRepr"
representationOf _ = fail "not a reference"


firstOfRules :: Monad m => [Expression -> m (Maybe Expression)] -> Expression -> m (Maybe Expression)
firstOfRules [] _ = return Nothing
firstOfRules (r:rs) x = r x >>= maybe (firstOfRules rs x) (return . Just)

applicableRules :: (Functor m, MonadIO m) => Expression -> m [Expression]
applicableRules x = fmap concat $ sequence [ r x | r <- allRules ]


allRules :: (Functor m, MonadIO m) => [Expression -> m [Expression]]
allRules =
    [ rule_ChooseRepr
    , liftM maybeToList . rule_TrueIsNoOp
    , liftM maybeToList . rule_ToIntIsNoOp
    , liftM maybeToList . rule_InlineFilterInsideMap
    , liftM maybeToList . rule_TupleIndex
    , liftM maybeToList . rule_SetIn_Explicit
    , liftM maybeToList . rule_SetIn_Occurrence
    , liftM maybeToList . rule_SetIn_ExplicitVarSizeWithMarker
    , liftM maybeToList . rule_SetIn_ExplicitVarSizeWithFlags
    ]


rule_ChooseRepr :: Monad m => Expression -> m [Expression]
rule_ChooseRepr (Reference nm (Just (DeclNoRepr ty _ inpDom))) = do
    let domOpts = reprOptions inpDom
    when (null domOpts) $
        bug $ "No representation matches this beast:" <++> pretty inpDom
    return [ Reference nm (Just (DeclHasRepr ty nm dom))
           | dom <- domOpts
           ]
rule_ChooseRepr _ = return []

rule_TrueIsNoOp :: Monad m => Expression -> m (Maybe Expression)
rule_TrueIsNoOp = return . theRule
    where
        theRule (Op (MkOpTrue (OpTrue _))) = Just $ Constant $ ConstantBool True
        theRule _ = Nothing


rule_ToIntIsNoOp :: Monad m => Expression -> m (Maybe Expression)
rule_ToIntIsNoOp = return . theRule
    where
        theRule (Op (MkOpToInt (OpToInt b))) = Just b
        theRule _ = Nothing


rule_InlineFilterInsideMap :: Monad m => Expression -> m (Maybe Expression)
rule_InlineFilterInsideMap = return . theRule
    where
        theRule (Op (MkOpMapOverDomain (OpMapOverDomain
                        (Lambda vBody body)
                        (Op (MkOpFilter (OpFilter
                                (Lambda vGuard guard_)
                                domain)))))) =
            let
                fGuard  = lambdaToFunction vGuard guard_
                fBody   = lambdaToFunction vBody  body
                newBody = Lambda vBody (Op $ MkOpAnd $ OpAnd [fGuard vBody, fBody vBody])
            in
                Just $ Op $ MkOpMapOverDomain $ OpMapOverDomain newBody domain
        theRule _ = Nothing


rule_TupleIndex :: (Functor m, Monad m) => Expression -> m (Maybe Expression)
rule_TupleIndex p = runMaybeT $ do
    (t,i)       <- match opIndexing p
    TypeTuple{} <- typeOf t
    iInt        <- match constantInt i
    ts          <- downX1 t
    return (atNote "Tuple indexing" ts (iInt-1))


rule_SetIn_Explicit :: (Functor m, MonadIO m) => Expression -> m (Maybe Expression)
rule_SetIn_Explicit p = runMaybeT $ do
    (x,s)                <- match opIn p
    TypeSet{}            <- typeOf s
    "Explicit"           <- representationOf s
    [m]                  <- downX1 s
    DomainMatrix index _ <- domainOf (Proxy :: Proxy ()) m
    -- exists i : index . m[i] = x
    -- or([ m[i] = x | i : index ])
    -- or(map_domain(i --> m[i]))
    let body = mkLambda "i" TypeInt $ \ i ->
                    make opEq (make opIndexing m i) x
    return $ make opOr [make opMapOverDomain body (Domain index)]


rule_SetIn_Occurrence :: (Functor m, MonadIO m) => Expression -> m (Maybe Expression)
rule_SetIn_Occurrence p = runMaybeT $ do
    (x,s)                <- match opIn p
    TypeSet{}            <- typeOf s
    "Occurrence"         <- representationOf s
    [m]                  <- downX1 s
    return $ make opIndexing m x


rule_SetIn_ExplicitVarSizeWithMarker :: (Functor m, MonadIO m) => Expression -> m (Maybe Expression)
rule_SetIn_ExplicitVarSizeWithMarker p = runMaybeT $ do
    (x,s)                       <- match opIn p
    TypeSet{}                   <- typeOf s
    "ExplicitVarSizeWithMarker" <- representationOf s
    [marker,values]             <- downX1 s
    DomainMatrix index _        <- domainOf (Proxy :: Proxy ()) values
    -- exists i : index , i < marker. m[i] = x
    -- exists i : index . i < marker /\ m[i] = x
    -- or([ i < marker /\ m[i] = x | i : index ])
    -- or(map_domain(i --> i < marker /\ m[i] = x))
    let body = mkLambda "i" TypeInt $ \ i ->
                make opAnd [ make opEq (make opIndexing values i) x
                           , make opLt i marker
                           ]
    return $ make opOr [make opMapOverDomain body (Domain index)]


rule_SetIn_ExplicitVarSizeWithFlags :: (Functor m, MonadIO m) => Expression -> m (Maybe Expression)
rule_SetIn_ExplicitVarSizeWithFlags p = runMaybeT $ do
    (x,s)                       <- match opIn p
    TypeSet{}                   <- typeOf s
    "ExplicitVarSizeWithFlags"  <- representationOf s
    [flags,values]              <- downX1 s
    DomainMatrix index _        <- domainOf (Proxy :: Proxy ()) values
    -- exists i : index , i < marker. m[i] = x
    -- exists i : index . i < marker /\ m[i] = x
    -- or([ i < marker /\ m[i] = x | i : index ])
    -- or(map_domain(i --> flags[i] /\ m[i] = x))
    let body = mkLambda "i" TypeInt $ \ i ->
                make opAnd [ make opEq (make opIndexing values i) x
                           , make opIndexing flags i
                           ]
    return $ make opOr [make opMapOverDomain body (Domain index)]

