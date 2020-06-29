module Conjure.Process.Features ( calculateFeatures ) where

import Conjure.Prelude
import Conjure.Language

import qualified Data.HashMap.Strict as M   -- containers

-- primes
import Data.Numbers.Primes ( isPrime )


-- Ignoring the model for now
calculateFeatures ::
    MonadIO m =>
    MonadUserError m =>
    Model -> Model -> m ()
calculateFeatures model param = do
    let domains = M.fromList [ (name, domain)
                             | Declaration (FindOrGiven Given name domain) <- mStatements model
                             ]
    let allIntValues = [ value | Declaration (Letting _ (Constant (ConstantInt _ value))) <- mStatements param ]
    forM_ (mStatements param) $ \ st ->
        case st of
            Declaration (Letting nm (Constant value)) ->
                case M.lookup nm domains of
                    Just domain -> onDomain allIntValues nm domain value
                    Nothing -> userErr1 $ vcat [ "Extra letting in the parameter file:" <+> pretty nm
                                               , "                        with domain:" <+> pretty value
                                               ]
            _ -> return ()
    forM_ (mStatements param) $ \ st1 ->
        case st1 of
            Declaration (Letting nm1 (Constant value1)) ->
                case M.lookup nm1 domains of
                    Nothing -> return ()
                    Just domain1 -> do
                        forM_ (mStatements param) $ \ st2 ->
                            case st2 of
                                Declaration (Letting nm2 (Constant value2)) ->
                                    case M.lookup nm2 domains of
                                        Nothing -> return ()
                                        Just domain2 -> onDomain2 nm1 domain1 value1 nm2 domain2 value2
                                _ -> return ()
            _ -> return ()

emit ::
    MonadIO m =>
    Pretty c =>
    [Name] -> c -> m ()
emit names value = liftIO $ putStrLn $ renderWide $ pretty (mconcat (intersperse "_" names)) <> ":" <+> pretty value


-- linear

type FeatureGenerator1 m = Name -> Domain () Expression -> Constant -> m ()

onDomain :: MonadIO m => [Integer] -> FeatureGenerator1 m
onDomain allIntValues name domain value =
    mapM_ (\ g -> g name domain value) (generators1 allIntValues)

generators1 :: MonadIO m => [Integer] -> [FeatureGenerator1 m]
generators1 allIntValues =
    [ intValue, intIsEven, intIsSquare, intIsPrime
    , intIsOffByOne allIntValues, intIsRepeated allIntValues
    ]

intValue :: MonadIO m => FeatureGenerator1 m
intValue name _ (ConstantInt _ value) =
    emit [name, "intValue"] value
intValue _ _ _ = return ()

intIsEven :: MonadIO m => FeatureGenerator1 m
intIsEven name _ (ConstantInt _ value) =
    emit [name, "intIsEven"] (mod value 2 == 0)
intIsEven _ _ _ = return ()

intIsSquare :: MonadIO m => FeatureGenerator1 m
intIsSquare name _ (ConstantInt _ value) =
    emit [name, "intIsSquare"] (round ((sqrt (fromIntegral value)) ** 2 :: Double) == value)
intIsSquare _ _ _ = return ()

intIsPrime :: MonadIO m => FeatureGenerator1 m
intIsPrime name _ (ConstantInt _ value) =
    emit [name, "intIsPrime"] (isPrime value)
intIsPrime _ _ _ = return ()

intIsOffByOne :: MonadIO m => [Integer] -> FeatureGenerator1 m
intIsOffByOne allIntValues name _ (ConstantInt _ value) =
    emit [name, "intIsOffByOne"] (any (\ v -> abs (value - v) == 1) allIntValues)
intIsOffByOne _ _ _ _ = return ()

intIsRepeated :: MonadIO m => [Integer] -> FeatureGenerator1 m
intIsRepeated allIntValues name _ (ConstantInt _ value) =
    emit [name, "intIsRepeated"] (any (\ v -> value == v) allIntValues)
intIsRepeated _ _ _ _ = return ()


-- quadratic

type FeatureGenerator2 m = Name -> Domain () Expression -> Constant ->
                           Name -> Domain () Expression -> Constant -> m ()

onDomain2 :: MonadIO m => FeatureGenerator2 m
onDomain2 name1 domain1 value1 name2 domain2 value2 =
    mapM_ (\ g -> g name1 domain1 value1 name2 domain2 value2) generators2

generators2 :: MonadIO m => [FeatureGenerator2 m]
generators2 =
    [ intIntRatio
    ]

intIntRatio :: MonadIO m => FeatureGenerator2 m
intIntRatio x _ (ConstantInt _ xVal) y _ (ConstantInt _ yVal) | x /= y =
    emit [x, y, "intIntRatio"] (fromIntegral xVal / fromIntegral yVal :: Double)
intIntRatio _ _ _ _ _ _ = return ()


