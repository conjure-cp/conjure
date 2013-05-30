{-# OPTIONS_GHC -fno-warn-orphans #-}
{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE CPP #-}

module Language.E.CompE where

import Bug
import Stuff.Funky.FunkySingle
import Stuff.Funky.FunkyMulti
import Stuff.NamedLog
import Stuff.MonadList

import Conjure.Mode

import Language.E.Imports
import Language.E.Definition
import Language.E.Pretty

import qualified Data.HashSet as S
import qualified Data.HashMap.Strict as M
import Data.IntMap ( IntMap )
import Data.IntSet ( IntSet )

import qualified GHC.Generics ( Generic )



class ( Functor m
      , Applicative m
      , Monad m
      , MonadState ConjureState m
      , MonadError ConjureError m
      ) => MonadConjure m where
    type ResultF m a
    runFunky :: m a -> ResultF m a


instance MonadConjure (FunkySingle ConjureState ConjureError Identity) where
    type ResultF      (FunkySingle ConjureState ConjureError Identity) a =
                      (Either ConjureError a, ConjureState)
    runFunky ma = runIdentity $ runFunkySingle def ma

instance MonadConjure (FunkyMulti  GlobalState ConjureState ConjureError Identity) where
    type ResultF      (FunkyMulti  GlobalState ConjureState ConjureError Identity) a =
                      [(Either ConjureError a, ConjureState)]
    runFunky ma = fst $ runIdentity $ runFunkyMulti def def ma


instance MonadConjure (FunkySingle ConjureState ConjureError IO) where
    type ResultF      (FunkySingle ConjureState ConjureError IO) a =
                      IO (Either ConjureError a, ConjureState)
    runFunky = runFunkySingle def

instance MonadConjure (FunkyMulti  GlobalState ConjureState ConjureError IO) where
    type ResultF      (FunkyMulti  GlobalState ConjureState ConjureError IO) a =
                      IO [(Either ConjureError a, ConjureState)]
    runFunky ma = liftM fst $ runFunkyMulti def def ma


class ( MonadConjure m
      , MonadList m
      , RandomM m
      ) => MonadConjureList m where
    getsGlobal :: (GlobalState -> a) -> m a
    modifyGlobal :: (GlobalState -> GlobalState) -> m ()


instance MonadConjureList (FunkyMulti GlobalState ConjureState ConjureError Identity) where
    getsGlobal = fmGetsGlobal
    modifyGlobal = fmModifyGlobal

instance MonadConjureList (FunkyMulti GlobalState ConjureState ConjureError IO) where
    getsGlobal = fmGetsGlobal
    modifyGlobal = fmModifyGlobal


runCompE
    :: String
    -> FunkyMulti GlobalState ConjureState ConjureError Identity a
    -> [(Either Doc a, LogTree)]
runCompE d ma = afterCompERun d $ runFunky ma


runCompEIO
    :: String
    -> FunkyMulti GlobalState ConjureState ConjureError IO a
    -> IO [(Either Doc a, LogTree)]
runCompEIO d ma = afterCompERun d <$> runFunky ma


runCompESingle
    :: String
    -> FunkySingle ConjureState ConjureError Identity a
    -> (Either Doc a, LogTree)
runCompESingle d ma = afterCompERunSingle d $ runFunky ma


runCompEIOSingle
    :: String
    -> FunkySingle ConjureState ConjureError IO a
    -> IO (Either Doc a, LogTree)
runCompEIOSingle d ma = afterCompERunSingle d <$> runFunky ma


afterCompERun
    :: String
    -> [(Either ConjureError a, ConjureState)]
    -> [(Either Doc          a, LogTree     )]
afterCompERun d = map (afterCompERunSingle d)

afterCompERunSingle
    :: String
    -> (Either ConjureError a, ConjureState)
    -> (Either Doc          a, LogTree     )
afterCompERunSingle d (Left  x, st) = (Left $ toError d x, localLogs st)
afterCompERunSingle _ (Right x, st) = (Right x           , localLogs st)

handleInIO
    :: [(Either Doc a, LogTree)]
    -> IO [a]
handleInIO = mapM handleInIOSingle

handleInIOSingle
    :: (Either Doc a, LogTree)
    -> IO a
handleInIOSingle (mx, logs) = do
    printLogs logs
    case mx of
        Left  x -> userErr x
        Right x -> return x

toError :: String -> ConjureError -> Doc
toError msg = prettyError (pretty msg)



-- errors

type ConjureError = (ErrEnum, Doc, Maybe Spec)

data ErrEnum = ErrFatal        -- means execution cannot continue.
             | ErrGeneratesNone
    deriving (Eq, Show)

err :: MonadConjure m => ErrEnum -> Doc -> m a
err e d = do
    sp <- gets lastSpec
    throwError (e,d,sp)

prettyError :: Doc -> ConjureError -> Doc
prettyError msg (_, d, Nothing) = vcat [msg, nest 4 d, ""]
prettyError msg (_, d, Just sp) = vcat [msg, nest 4 d, nest 4 (pretty sp), ""] 

instance Pretty ConjureError where
    pretty (_, d, Nothing) = vcat [ nest 4 d,                     "" ]
    pretty (_, d, Just sp) = vcat [ nest 4 d, nest 4 (pretty sp), "" ]

recordSpec :: MonadConjure m => Doc -> Spec -> m Spec
recordSpec _msg sp = do
#ifdef TRACELOGS
    sp' <- gets lastSpec
    unless (Just sp == sp') $ mkLog "recordSpec" $ vcat [ _msg, pretty sp ]
#endif
    modify $ \ st -> st { lastSpec = Just sp }
    return sp


-- state

data ConjureState = ConjureState
        { binders       :: ![Binder]
        , uniqueNameInt :: !Integer
        , representationLog :: ![ ( Text     -- original name
                                  , Text     -- representation name
                                  , E        -- original full declaration
                                  , E        -- new domain
                                  ) ]
        , structuralConsLog :: ![ ( Text     -- representation name
                                  , [E]
                                  )
                                ]
        , lastSpec :: !(Maybe Spec) -- record the spec after changes, to report in case of an error.
        , localLogs :: !LogTree
        , allNamesPreConjure :: !(S.HashSet Text)  -- all identifiers used in the spec, pre conjure. to avoid name clashes.
        }

bindersDoc :: MonadConjure m => m Doc
bindersDoc = do
    bs <- gets binders
    return $ if null bs
        then ""
        else "Current bindings: " <+>
             vcat (map pretty $ nubBy ((==) `on` binderName) bs)

bindersDocNamesOnly :: MonadConjure m => m Doc
bindersDocNamesOnly = do
    bs <- gets binders
    return $ if null bs
        then ""
        else "Current bindings: " <+>
             prettyList id "," (nub $ map binderName bs)

data Binder = Binder !Text !E
    deriving (Show, GHC.Generics.Generic)

binderName :: Binder -> Text
binderName (Binder nm _) = nm

instance Hashable Binder where

instance Default (S.HashSet a) where
    def = S.empty

instance Default ConjureState where
    def = ConjureState def 1 def def def def def

instance Pretty Binder where
    pretty (Binder nm val) = pretty nm <+> ":" <+> pretty val

mkLog :: MonadConjure m => String -> Doc -> m ()
mkLog nm doc = case buildLog nm doc of
    Nothing -> return ()
    Just l  -> modify $ \ st -> st {
        localLogs = LTMultiple (localLogs st) (LTSingle l)
        }

addReference :: MonadConjure m => Text -> E -> m ()
addReference nm val = modify $ \ st -> st { binders = Binder nm val : binders st }

addMetaVar :: MonadConjure m => Text -> E -> m ()
addMetaVar nm = addReference ("&" `mappend` nm)

lookupReference :: MonadConjure m => Text -> MaybeT m E
lookupReference nm
    | nm `elem` ["forAll", "exists", "sum"]
    = return [xMake| reference := [Prim (S nm)] |]
lookupReference nm = do
    let (base,_,_) = identifierSplit nm
    bs <- lift $ gets binders
    case listToMaybe [ x | Binder nm' x <- bs, base == nm' ] of
        Nothing -> mzero
        Just x  -> return x

lookupMetaVar :: MonadConjure m => Text -> MaybeT m E
lookupMetaVar nm = lookupReference ("&" `mappend` nm)

errUndefinedRef :: (MonadConjure m, Pretty p) => Doc -> p -> m a
errUndefinedRef _place t = do
    bsText <- bindersDocNamesOnly
    userErr $ vcat [ "Undefined reference:" <+> pretty t
                   , nest 4 bsText
                   ]

errMaybeT :: MonadConjure m => Doc -> (Text -> MaybeT m E) -> Text -> m E
errMaybeT place comp t = do
    ma <- runMaybeT (comp t)
    case ma of
        Just i  -> return i
        Nothing -> errUndefinedRef place t

nextUniqueName :: MonadConjure m => m Text
nextUniqueName = do
    i <- gets uniqueNameInt
    modify $ \ st -> st { uniqueNameInt = i + 1 }
    let nm = stringToText ("v__" ++ show i)
    nms <- gets allNamesPreConjure
    if nm `S.member` nms
        then nextUniqueName
        else return nm


data GlobalState = GlobalState
        { memoRefnChanged      :: !(IntMap E)
        , memoRefnStaysTheSame :: !IntSet
        , conjureMode          :: ConjureModeWithFlags
        , conjureSeed          :: StdGen
        }

instance (Functor m, Monad m) => RandomM (FunkyMulti GlobalState st err m) where
    get_stdgen = fmGetsGlobal conjureSeed
    set_stdgen x = fmModifyGlobal $ \ gl -> gl { conjureSeed = x }

rangeRandomM :: RandomM m => (Int, Int) -> m Int
rangeRandomM range = do
    gen <- get_stdgen
    let (x, gen') = randomR range gen
    set_stdgen gen'
    return x


instance Default GlobalState where
    def = GlobalState def def
            (ConjureModeWithFlags ModeUnknown M.empty def def)
            (error "Seed not initialised")

makeIdempotent :: Monad m => (a -> m (a,Bool)) -> a -> m a
makeIdempotent f x = do
    (y,flag) <- f x
    if flag
        then makeIdempotent f y
        else return y


class SelectByMode a where
    selectByMode :: RandomM m => ConjureModeWithFlags -> [a] -> m [a]
    selectByMode (ConjureModeWithFlags m _ _ _) = defSelectByMode m

defSelectByMode :: RandomM m => ConjureMode -> [a] -> m [a]
defSelectByMode _                                   [] = return []
defSelectByMode (ModeUnknown                    {}) xs = return xs
defSelectByMode (ModeMultipleOutput             {}) xs = return xs
defSelectByMode (ModeSingleOutput ModeFirst  _ _  ) (x:_) = return [x]
defSelectByMode (ModeSingleOutput ModeRandom _ _  ) xs = do
    i <- rangeRandomM (0, length xs - 1)
    return [xs !! i]
defSelectByMode (ModeSingleOutput               {}) (x:_) = return [x]
defSelectByMode _ _ = error "selectByMode: Shouldn't be used in this mode"

instance SelectByMode E where
    selectByMode _ [] = return []
    selectByMode (ConjureModeWithFlags mode _ _ _) xs =
        case mode of
            ModeSingleOutput ModeCompact _ _ -> return [minimumBy (comparing eDepth) xs]
            _                                -> defSelectByMode mode xs

eDepth :: E -> Int
eDepth (Tagged _ []) = 1
eDepth (Tagged _ ys) = 1 + maximum (map eDepth ys)
eDepth _ = 1

compareChain :: [Ordering] -> Ordering
compareChain (EQ:xs) = compareChain xs
compareChain (x :_ ) = x
compareChain []      = EQ

domOrder :: E -> E -> Ordering
domOrder
    [xMatch| _ := domain.bool |]
    [xMatch| _ := domain.bool |] = EQ
domOrder
    [xMatch| _ := domain.int |]
    [xMatch| _ := domain.int |] = EQ
domOrder
    [xMatch| [indexA] := domain.matrix.index
           | [innerA] := domain.matrix.inner
           |]
    [xMatch| [indexB] := domain.matrix.index
           | [innerB] := domain.matrix.inner
           |]
    = compareChain [ domOrder indexA indexB
                   , domOrder innerA innerB
                   ]
domOrder [xMatch| _ := domain.bool   |] _ = LT
domOrder _ [xMatch| _ := domain.bool   |] = GT
domOrder [xMatch| _ := domain.int    |] _ = LT
domOrder _ [xMatch| _ := domain.int    |] = GT
domOrder [xMatch| _ := domain.matrix |] _ = LT
domOrder _ [xMatch| _ := domain.matrix |] = GT

domOrder x y = compare (eDepth x) (eDepth y)

compactSelect :: [RuleReprResult] -> RuleReprResult
compactSelect = minimumBy comparer
    where
        comparer ( _origDecl1, _ruleName1, _reprName1, newDom1, structuralCons1)
                 ( _origDecl2, _ruleName2, _reprName2, newDom2, structuralCons2) =
            compareChain
                [ domOrder newDom1 newDom2
                , compare (length structuralCons1) (length structuralCons2)
                ]

-- uses compactSelect if the argument is a given,
-- DfAll if the argument is a find.
compactIfParam :: [RuleReprResult] -> [RuleReprResult]
compactIfParam xs@(([xMatch| _ := topLevel.declaration.given |], _, _, _, _):_) = [compactSelect xs]
compactIfParam xs = xs


instance SelectByMode RuleReprResult where
    selectByMode _ [] = return []
    selectByMode (ConjureModeWithFlags mode _ _ _) xs =
        case mode of
            ModeSingleOutput ModeCompact      _ _ -> return [compactSelect xs]
            ModeMultipleOutput DFCompactParam _ _ -> return (compactIfParam xs)
            _ -> defSelectByMode mode xs


type ReprFunc m =
    ( Text                                  -- input: name of the variable
    , E                                     -- input: domain
    , E                                     -- input: decl
    )
    -> m [RuleReprResult]


type RefnFunc m =
    E                                   -- the expression
    -> m (Maybe [(Text, E)])            -- Nothing if rule doesn't apply
                                        -- returns a list of rewrites, fst being rulename
                                        --                           , snd being E


