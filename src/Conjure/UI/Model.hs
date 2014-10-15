{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE ParallelListComp #-}

module Conjure.UI.Model where

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
import Conjure.Representations

import Data.Generics.Uniplate.Data ( rewriteBiM, uniplate, biplate )
import Data.Generics.Str ( Str )
import qualified Data.Text as T


data ModelGen = ModelGen
    { originalEssence :: Model
    , history :: [ModelInfo]
    }

initialise :: Model -> ModelGen
initialise m = ModelGen (addTrueConstraints m) []

-- | repeatedly call `nextModel` to generate all models
outputAllModels :: FilePath -> Int -> ModelGen -> IO ()
outputAllModels dir i gen = do
    putStrLn $ "Working on model #" ++ show i
    createDirectoryIfMissing True dir
    may <- nextModel gen
    case may of
        Nothing -> return ()
        Just (eprime,gen') -> do
            let filename = dir </> "model" ++ show i ++ ".eprime"
            writeFile filename (renderWide eprime)
            print $ vcat
                [ pretty sel <+> "out of" <+> pretty (show opts) <+> "~~" <+> vcat (map pretty txts)
                | Decision txts opts sel <- miTrail (mInfo eprime)
                ]
            -- outputAllModels dir (i+1) gen'

-- | given a `ModelGen`, which contains info about previously generated models,
--   generate the next model.
nextModel :: ModelGen -> IO (Maybe (Model, ModelGen))
nextModel (ModelGen essence pastInfos) = do
    meprime <- genNextModel essence pastInfos                       -- the workhorse
    case meprime of
        Nothing -> return Nothing                                   -- no more models to be generated
        Just eprime -> do
            let info = mInfo eprime
            return (Just ( languageEprime (oneSuchThat eprime)      -- return the newly generated model
                         , ModelGen essence (info:pastInfos)        -- and add its "info" to the log
                         ))

-- | given an initial essence model,
--   and a list of ModelInfo's describing previously generated models,
--   generate the next model
--   or return Nothing if no other model can be generated
genNextModel :: Model -> [ModelInfo] -> IO (Maybe Model)
genNextModel initialEssence pastInfos = do

    let lets  = lettings initialEssence
    let decls = declarations initialEssence

    let
        reportNode :: (MonadState St m, MonadIO m) => Expression -> m ()
        reportNode x = do
            gets stNbExpression >>= \ nb -> liftIO $ print $ "--" <+> pretty nb <> ":" <+> pretty (show x)
            modify $ \ st -> st { stNbExpression = 1 + stNbExpression st }

    let
        f :: (MonadState St m, MonadIO m) => Expression -> m Expression
        f (Reference nm Nothing) =
            case lookup nm decls of
                Nothing ->
                    case lookup nm lets of
                        Nothing -> do
                            liftIO $ putStrLn $ "what's this a reference to? " ++ show nm
                            return (Reference nm Nothing)
                        Just _  -> return (Reference nm Nothing)
                Just inpDom -> do
                    ascendants <- reportAscendants
                    explored <- gets alreadyExplored

                    let domOpts = reprOptions inpDom
                    let numOptions = [1 .. length domOpts]

                    when (null domOpts) $
                        bug $ "No representation matches this beast:" <++> pretty inpDom

                    case numOptions \\ explored of
                        [] -> do
                            modify $ \ st -> st { stExhausted = True }
                            liftIO $ putStrLn "exhausted=true"
                            -- st <- gets id
                            -- liftIO $ print st
                            return (Reference nm Nothing)
                        _ -> do
                            liftIO $ print $ vcat
                                $ ("Selecting representation for:" <+> pretty nm)
                                : map (nest 4) (
                                    [ "Options" <+> (vcat [ nest 4 (pretty i <> ":" <+> pretty d)
                                                          | i <- allNats
                                                          | d <- domOpts
                                                          ])
                                    ] ++ ascendants )
                            ln <- liftIO getLine
                            let (numSelected, domSelected) =
                                    case readMay ln of
                                        Nothing -> userErr "You've got to enter an integer."
                                        Just n -> (n, domOpts `at` (n - 1))
                            let descr = vcat
                                    $ ("Selecting representation for:" <+> pretty nm)
                                    : map (nest 4) (
                                       [ "Options: " <+> (vcat (map (nest 4 . pretty) domOpts))
                                       , "Selected:" <+> pretty domSelected
                                       , "# Options: " <+> pretty (show numOptions)
                                       , "# Explored:" <+> pretty (show explored)
                                       , "# Selected:" <+> pretty numSelected
                                       ] ++ ascendants )
                            modify $ addReprToSt nm domSelected
                            modify $ addDecisionToSt descr numOptions numSelected
                            liftIO $ print descr
                            return (Reference nm (Just domSelected))
        f x = return x

    let initInfo = def { miGivens = map fst (givens initialEssence)
                       , miFinds  = map fst (finds  initialEssence)
                       }
    let pipeline =  tr (\ x -> do reportNode x; f x )
                >=> ifNotExhausted (rewriteBiM $ firstOfRules [ rule_TrueIsNoOp
                                                              , rule_ToIntIsNoOp
                                                              , rule_InlineFilterInsideMap
                                                              , rule_TupleIndex
                                                              , rule_SetIn_Explicit
                                                              , rule_SetIn_Occurrence
                                                              , rule_SetIn_ExplicitVarSizeWithMarker
                                                              , rule_SetIn_ExplicitVarSizeWithFlags
                                                              ]
                                   )
                >=> updateDeclarations
    (statements', st) <- runStateT (pipeline (mStatements initialEssence))
                                   (def { stCurrInfo = initInfo
                                        , stPastInfos = map miTrail pastInfos
                                        })

    if stExhausted st
        then return Nothing
        else do
            let model = initialEssence { mStatements = statements'
                                       , mInfo = stCurrInfo st
                                       }
            return (Just model)


class ExpressionContainer a where
    tr :: MonadState St m => (Expression -> m Expression) -> a -> m a

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


addReprToSt :: Name -> Domain HasRepresentation Expression -> St -> St
addReprToSt nm dom st = st { stCurrInfo = addToInfo (stCurrInfo st)
                           , stAllReprs = nub $ (nm, dom) : inners ++ stAllReprs st
                           }
    where
        addToInfo i = i { miRepresentations = nub $ (nm, dom) : miRepresentations i }
        inners = case mkInners (nm,dom) of
            Left err -> bug err
            Right res -> res
        mkInners p = do
            mmids <- down1_ p
            case mmids of
                Nothing -> return []
                Just mids -> do
                    lows <- mapM mkInners mids
                    return (concat (mids:lows))
            
addDecisionToSt :: Doc -> [Int] -> Int -> St -> St
addDecisionToSt doc opts selected st =
    st { stCurrInfo = addToInfo (stCurrInfo st)
       , stPastInfos = advancePastInfos (stPastInfos st)
       }
    where addToInfo i = i { miTrail = miTrail i ++ [dec] }
          dec = Decision (doc |> renderWide |> stringToText |> T.lines) opts selected
          advancePastInfos trails =
              [ tail trail                      -- we drop the head to advance in the trail
              | trail <- trails
              , not (null trail)                -- check if this trail is already exhausted
              , let this = head trail
              , dDecision this == selected      -- only those which picked the same option are relevant.
              ]

reportAscendants :: MonadState St m => m [Doc]
reportAscendants = do
    contexts <- gets stAscendants
    return
        [ "Context #" <> pretty i <> ":" <+> either pretty pretty c
        | (i,c) <- zip allNats contexts
        ]

alreadyExplored :: St -> [Int]
alreadyExplored st =
    [ dDecision (head trail)
    | trail <- stPastInfos st
    , not (null trail)
    ]

ifNotExhausted :: MonadState St m => (a -> m a) -> a -> m a
ifNotExhausted f x = do
    exhausted <- gets stExhausted
    if exhausted
        then return x
        else f x

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


updateDeclarations :: (Functor m, MonadState St m) => [Statement] -> m [Statement]
updateDeclarations statements = do
    reprs <- gets stAllReprs
    flip concatMapM statements $ \ st ->
        case st of
            Declaration (FindOrGiven h nm _) ->
                case [ d | (n,d) <- reprs, n == nm ] of
                    [] -> bug $ "No representation chosen for: " <+> pretty nm
                    domains -> flip concatMapM domains $ \ domain -> do
                        mouts <- runExceptT $ down_ (nm, domain)
                        case mouts of
                            Left err -> bug err
                            Right outs -> return [Declaration (FindOrGiven h n (forgetRepr d)) | (n,d) <- outs]
            _ -> return [st]


representationOf :: (MonadFail m, MonadState St m) => Expression -> m Name
representationOf (Reference _ Nothing) = fail "doesn't seem to have a representation"
representationOf (Reference _ (Just d)) =
    case reprAtTopLevel d of
        Nothing -> fail "doesn't seem to have a representation"
        Just NoRepresentation -> fail "doesn't seem to have a representation"
        Just (HasRepresentation r) -> return r
representationOf _ = fail "not a reference"


firstOfRules :: Monad m => [Expression -> m (Maybe Expression)] -> Expression -> m (Maybe Expression)
firstOfRules [] _ = return Nothing
firstOfRules (r:rs) x = r x >>= maybe (firstOfRules rs x) (return . Just)


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


rule_TupleIndex :: (Functor m, MonadState St m) => Expression -> m (Maybe Expression)
rule_TupleIndex p = runMaybeT $ do
    (t,i)       <- match opIndexing p
    TypeTuple{} <- typeOf t
    iInt        <- match constantInt i
    ts          <- down1X t
    return (atNote "Tuple indexing" ts (iInt-1))


rule_SetIn_Explicit :: (Functor m, MonadState St m, MonadIO m) => Expression -> m (Maybe Expression)
rule_SetIn_Explicit p = runMaybeT $ do
    (x,s)                <- match opIn p
    TypeSet{}            <- typeOf s
    "Explicit"           <- representationOf s
    [m]                  <- down1X s
    DomainMatrix index _ <- domainOf (Proxy :: Proxy ()) m
    -- exists i : index . m[i] = x
    -- or([ m[i] = x | i : index ])
    -- or(map_domain(i --> m[i]))
    let i    = "i" :: Name
    let body = Lambda (Single i TypeInt)
                      (make opEq (make opIndexing m (Reference i Nothing)) x)
    return $ make opOr [make opMapOverDomain body (Domain index)]


rule_SetIn_Occurrence :: (Functor m, MonadState St m, MonadIO m) => Expression -> m (Maybe Expression)
rule_SetIn_Occurrence p = runMaybeT $ do
    (x,s)                <- match opIn p
    TypeSet{}            <- typeOf s
    "Occurrence"         <- representationOf s
    [m]                  <- down1X s
    return $ make opIndexing m x


rule_SetIn_ExplicitVarSizeWithMarker :: (Functor m, MonadState St m, MonadIO m) => Expression -> m (Maybe Expression)
rule_SetIn_ExplicitVarSizeWithMarker p = runMaybeT $ do
    (x,s)                       <- match opIn p
    TypeSet{}                   <- typeOf s
    "ExplicitVarSizeWithMarker" <- representationOf s
    [marker,values]             <- down1X s
    DomainMatrix index _        <- domainOf (Proxy :: Proxy ()) values
    -- exists i : index , i < marker. m[i] = x
    -- exists i : index . i < marker /\ m[i] = x
    -- or([ i < marker /\ m[i] = x | i : index ])
    -- or(map_domain(i --> i < marker /\ m[i] = x))
    let i    = "i" :: Name
    let body = Lambda (Single i TypeInt)
                      (make opAnd [ make opEq (make opIndexing values (Reference i Nothing)) x
                                  , make opLt (Reference i Nothing) marker
                                  ])
    return $ make opOr [make opMapOverDomain body (Domain index)]


rule_SetIn_ExplicitVarSizeWithFlags :: (Functor m, MonadState St m, MonadIO m) => Expression -> m (Maybe Expression)
rule_SetIn_ExplicitVarSizeWithFlags p = runMaybeT $ do
    (x,s)                       <- match opIn p
    TypeSet{}                   <- typeOf s
    "ExplicitVarSizeWithFlags"  <- representationOf s
    [flags,values]              <- down1X s
    DomainMatrix index _        <- domainOf (Proxy :: Proxy ()) values
    -- exists i : index , i < marker. m[i] = x
    -- exists i : index . i < marker /\ m[i] = x
    -- or([ i < marker /\ m[i] = x | i : index ])
    -- or(map_domain(i --> flags[i] /\ m[i] = x))
    let i    = "i" :: Name
    let body = Lambda (Single i TypeInt)
                      (make opAnd [ make opEq (make opIndexing values (Reference i Nothing)) x
                                  , make opIndexing flags (Reference i Nothing)
                                  ])
    return $ make opOr [make opMapOverDomain body (Domain index)]


getName :: Expression -> Maybe (Name, Name -> Expression)
getName (Reference nm d) = Just (nm, \ n -> Reference n d)
getName (Op (MkOpIndexing (OpIndexing m i))) = do
    (nm, f) <- getName m
    return (nm, \ nm' -> Op (MkOpIndexing (OpIndexing (f nm') i)))
getName _ = Nothing


tupleIndex :: MonadState St m => Expression -> [a] -> Int -> m a
tupleIndex p xs i' = do
    let i = i' - 1
    if i >= 0 && i < length xs
        then return (xs `at` i)
        else do
            ascendants <- reportAscendants
            bug $ vcat
                $ ("tuple indexing out of bounds: " <++> pretty p)
                : ascendants


