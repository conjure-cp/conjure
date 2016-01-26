module Conjure.Process.Enumerate
    ( EnumerateDomain
    , enumerateDomain
    , enumerateInConstant
    , EnumerateDomainNoIO(..)
    ) where

import Conjure.Prelude
import Conjure.Language.AdHoc
import Conjure.UserError
import Conjure.Language.AbstractLiteral
import Conjure.Language.Constant
import Conjure.Language.Domain
import Conjure.Language.Pretty
import Conjure.Language.Definition
import Conjure.Language.NameGen

import Conjure.UI.IO
import Conjure.UI as UI ( UI(..) )
import {-# SOURCE #-} Conjure.UI.MainHelper

-- temporary
import System.IO.Temp ( withSystemTempDirectory )

-- pipes
import qualified Pipes


-- | This class is only to track where `enumerateDomain` might get called.
--   It is essentially MonadIO, but doesn't allow arbitrary IO.
class (Functor m, Applicative m, Monad m, MonadUserError m) => EnumerateDomain m where liftIO' :: IO a -> m a
instance EnumerateDomain IO where liftIO' = id
instance EnumerateDomain m => EnumerateDomain (IdentityT m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (MaybeT m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (ExceptT m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (ReaderT r m) where liftIO' = lift . liftIO'
instance (EnumerateDomain m, Monoid w) => EnumerateDomain (WriterT w m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (StateT st m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (Pipes.Proxy a b c d m) where liftIO' = lift . liftIO'
instance EnumerateDomain m => EnumerateDomain (NameGenM m) where liftIO' = lift . liftIO'

-- | Use this if you don't want to allow a (EnumerateDomain m => m a) computation actually do IO.
data EnumerateDomainNoIO a = Done a | TriedIO | Failed Doc
    deriving (Eq, Show)

instance Functor EnumerateDomainNoIO where
    fmap _ (Failed msg) = Failed msg
    fmap _ TriedIO      = TriedIO
    fmap f (Done x)     = Done (f x)

instance Applicative EnumerateDomainNoIO where
    pure = return
    (<*>) = ap

instance Monad EnumerateDomainNoIO where
    return = Done
    Failed msg >>= _ = Failed msg
    TriedIO    >>= _ = TriedIO
    Done x     >>= f = f x

instance MonadFail EnumerateDomainNoIO where
    fail = Failed

instance MonadUserError EnumerateDomainNoIO where
    userErr docs = Failed (vcat $ "User error:" : docs)

instance NameGen EnumerateDomainNoIO where
    nextName _ = fail "nextName{EnumerateDomainNoIO}"
    exportNameGenState = fail "exportNameGenState{EnumerateDomainNoIO}"
    importNameGenState _ = fail "importNameGenState{EnumerateDomainNoIO}"

instance EnumerateDomain EnumerateDomainNoIO where liftIO' _ = TriedIO

enumerateDomainMax :: Int
enumerateDomainMax = 10000

enumerateDomain :: (MonadFail m, MonadUserError m, EnumerateDomain m) => Domain () Constant -> m [Constant]
enumerateDomain DomainBool = return [ConstantBool False, ConstantBool True]
enumerateDomain (DomainInt rs) = concatMapM enumerateRange rs
enumerateDomain (DomainEnum _dName (Just rs) _mp) = concatMapM enumerateRange rs
enumerateDomain (DomainTuple ds) = do
    inners <- mapM enumerateDomain ds
    return $ map (ConstantAbstract . AbsLitTuple) (sequence inners)
enumerateDomain (DomainMatrix (DomainInt indexDom) innerDom) = do
    inners <- enumerateDomain innerDom
    indexInts <- rangesInts indexDom
    return
        [ ConstantAbstract (AbsLitMatrix (DomainInt indexDom) vals)
        | vals <- replicateM (length indexInts) inners
        ]

enumerateDomain d | not (null [ () | ConstantUndefined{} <- universeBi d ]) = return []

-- the sledgehammer approach
enumerateDomain d = liftIO' $ withSystemTempDirectory ("conjure-enumerateDomain-" ++ show (hash d)) $ \ tmpDir -> do
    let model = Model { mLanguage = LanguageVersion "Essence" [1,0]
                      , mStatements = [Declaration (FindOrGiven Find "x" (fmap Constant d))]
                      , mInfo = def
                      }
    let essenceFile = tmpDir </> "out.essence"
    let outDir = tmpDir </> "outDir"
    writeModel PlainEssence (Just essenceFile) model
    ignoreLogs $ mainWithArgs Solve
        { UI.essence                    = essenceFile
        , validateSolutionsOpt          = False
        , outputDirectory               = outDir
        , savilerowOptions              = "-O0 -preprocess None -timelimit 60000 -num-solutions " ++ show enumerateDomainMax
        , minionOptions                 = "-cpulimit 60"
        , logLevel                      = LogNone
        -- default values for the rest
        , essenceParams                 = []
        , numberingStart                = 1
        , smartFilenames                = False
        , verboseTrail                  = False
        , rewritesTrail                 = False
        , logRuleFails                  = False
        , logRuleSuccesses              = False
        , logRuleAttempts               = False
        , logChoices                    = False
        , strategyQ                     = "f"
        , strategyA                     = "c"
        , representations               = Nothing
        , representationsFinds          = Nothing
        , representationsGivens         = Nothing
        , representationsAuxiliaries    = Nothing
        , representationsQuantifieds    = Nothing
        , representationsCuts           = Nothing
        , channelling                   = False
        , representationLevels          = True
        , seed                          = Nothing
        , limitModels                   = Nothing
        , limitTime                     = Nothing
        , outputBinary                  = False
        , lineWidth                     = 120
        }
    solutions   <- filter (".solution" `isSuffixOf`) <$> getDirectoryContents outDir
    when (length solutions >= enumerateDomainMax) $ userErr1 $ vcat
        [ "Enumerate domain: too many."
        , "Nb solutions found:" <+> pretty (length solutions)
        , "When working on domain:" <++> pretty d
        ]
    enumeration <- fmap concat $ forM solutions $ \ solutionFile -> do
        Model _ decls _ <- readModelFromFile (outDir </> solutionFile)
        let (enumeration, errs) = mconcat
                [ case decl of
                    Declaration (Letting "x" x) | Just c <- e2c x -> ([c], [])
                    _ -> ([], [decl])
                | decl <- decls ]
        if null errs
            then return enumeration
            else fail $ vcat $ "enumerateDomain, not Constants!"
                             : ("When working on domain:" <++> pretty d)
                             :  map pretty errs
                             ++ map (pretty . show) errs
    removeDirectoryRecursive outDir
    removeDirectoryRecursive tmpDir
    return enumeration


enumerateRange :: MonadFail m => Range Constant -> m [Constant]
enumerateRange (RangeSingle x) = return [x]
enumerateRange (RangeBounded (ConstantInt x) (ConstantInt y)) = return $ map ConstantInt [x..y]
enumerateRange RangeBounded{} = fail "enumerateRange RangeBounded"
enumerateRange RangeOpen{} = fail "enumerateRange RangeOpen"
enumerateRange RangeLowerBounded{} = fail "enumerateRange RangeLowerBounded"
enumerateRange RangeUpperBounded{} = fail "enumerateRange RangeUpperBounded"

enumerateInConstant :: MonadFail m => Constant -> m [Constant]
enumerateInConstant constant = case constant of
    ConstantAbstract (AbsLitMatrix _  xs) -> return xs
    ConstantAbstract (AbsLitSet       xs) -> return xs
    ConstantAbstract (AbsLitMSet      xs) -> return xs
    ConstantAbstract (AbsLitFunction  xs) -> return [ ConstantAbstract (AbsLitTuple [i,j]) | (i,j) <- xs ]
    ConstantAbstract (AbsLitSequence  xs) -> return [ ConstantAbstract (AbsLitTuple [i,j])
                                                    | (i',j) <- zip allNats xs
                                                    , let i = fromInt i'
                                                    ]
    ConstantAbstract (AbsLitRelation  xs) -> return $ map (ConstantAbstract . AbsLitTuple) xs
    ConstantAbstract (AbsLitPartition xs) -> return $ map (ConstantAbstract . AbsLitSet) xs
    TypedConstant c _                     -> enumerateInConstant c
    _ -> fail $ vcat [ "enumerateInConstant"
                     , "constant:" <+> pretty constant
                     ]
