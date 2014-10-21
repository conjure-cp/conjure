{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
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

import Data.Generics.Uniplate.Zipper as Zipper ( Zipper, zipperBi, fromZipper, hole, replaceHole, up )
-- import Data.Generics.Uniplate.Data ( uniplate, biplate, rewriteBiM )
-- import Data.Generics.Str ( Str )
import qualified Data.Text as T



type Question = (Expression, [Answer])
type Answer = (Expression, Model)
type Driver = (forall m . (MonadIO m, MonadFail m) => [Question] -> m Model)


remaining :: (Functor m, Applicative m, MonadIO m) => Model -> m [Question]
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


toCompletion :: (MonadIO m, MonadFail m) => Driver -> Model -> m Model
toCompletion driver model = do
    qs <- remaining model
    if null qs
        then model |> oneSuchThat |> return
        else do
            nextModel <- driver qs
            toCompletion driver nextModel


outputOneModel :: Driver -> FilePath -> Int -> Model -> IO ()
outputOneModel driver dir i essence = do
    createDirectoryIfMissing True dir
    eprime <- toCompletion driver (addTrueConstraints essence)
    let filename = dir </> "model" ++ show i ++ ".eprime"
    writeFile filename (renderWide eprime)


pickFirst :: MonadFail m => [Question] -> m Model
pickFirst [] = fail "pickFirst: No questions!"
pickFirst (question:_) =
    case snd question of
        [] -> fail "pickFirst: No answers!"
        (answer:_) -> return (snd answer)


interactive :: MonadIO m => [Question] -> m Model
interactive questions = liftIO $ do
    putStrLn ""
    putStrLn " ----------------------------------------"
    forM_ (zip allNats questions) $ \ (nQ,(q,as)) -> do
        print ("Question" <+> pretty nQ <> ":" <+> pretty q)
        forM_ (zip allNats as) $ \ (nA,a) -> do
            print (nest 4 $ "Answer" <+> pretty nA <> ":" <+> pretty (fst a))
            print (nest 8 $ pretty (snd a))
    putStr "Pick question: "
    pickedQ <- readNote "Expecting an integer." <$> getLine
    putStr "Pick answer: "
    pickedA <- readNote "Expecting an integer." <$> getLine
    return $ snd (at (snd (at questions
                              (pickedQ-1)))
                     (pickedA-1))


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

reportAscendants :: Zipper a Expression -> [Doc]
reportAscendants = rep 1 . Just
    where
        rep :: Int -> Maybe (Zipper a Expression) -> [Doc]
        rep _ Nothing  = []
        rep i (Just z) = ("Context #" <> pretty i <> ":" <+> pretty (hole z))
                       : rep (i+1) (Zipper.up z)


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


applicableRules :: (Applicative m, MonadIO m) => Expression -> m [Expression]
applicableRules x = concat <$> sequence [ r x | r <- allRules ]


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

