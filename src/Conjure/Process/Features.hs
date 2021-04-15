module Conjure.Process.Features ( calculateFeatures ) where

import Conjure.Prelude
import Conjure.Language

import Conjure.Process.Enumerate ( EnumerateDomain )
import Conjure.Language.Instantiate ( instantiateExpression )
import Conjure.Language.Expression.DomainSizeOf ( domainSizeOf )


import qualified Data.HashMap.Strict as M   -- containers

-- base
import Text.Printf ( printf )

-- primes
import Data.Numbers.Primes ( isPrime )

-- statistics
import Statistics.Sample ( mean, stdDev, harmonicMean, geometricMean, skewness, kurtosis )
import Statistics.Correlation ( pearson )

-- vector
import qualified Data.Vector as Vector
import qualified Data.Vector.Generic.Base as Base




-- Ignoring the model for now
calculateFeatures ::
    MonadIO m =>
    MonadUserError m =>
    MonadFail m =>
    EnumerateDomain m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    Model -> Model -> m ()
calculateFeatures model param = do
    let
        domains :: M.HashMap Name (Domain () Expression)
        domains = M.fromList [ (name, domain)
                             | Declaration (FindOrGiven Given name domain) <- mStatements model
                             ]

        values :: [(Name, Expression)]
        values = [ (name, Constant value)
                 | Declaration (Letting name (Constant value)) <- mStatements param
                 ]

        parameters :: [Param]
        parameters = [ (nm, M.lookup nm domains, value)
                     | Declaration (Letting nm (Constant value)) <- mStatements param
                     ]

        allIntValues :: [Integer]
        allIntValues = [ value | Declaration (Letting _ (Constant (ConstantInt _ value))) <- mStatements param ]

        allIndicators :: [Indicator]
        allIndicators = concat [ i p
                               | p <- parameters
                               , i <- allIndicatorsGens
                               ]

        allFeature1s :: [Feature]
        allFeature1s = catMaybes [ f allIntValues i
                                 | i <- allIndicators
                                 , f <- allFeatureGen1s
                                 ]

        allFeature2s :: [Feature]
        allFeature2s = catMaybes [ f allIntValues i j
                                 | i <- allIndicators
                                 , j <- allIndicators
                                 , fst i < fst j
                                 , f <- allFeatureGen2s
                                 ]

    allDirectFeatures
        :: [Feature]
        <- flip runReaderT values $ concat <$> sequence [ f p
                                                        | p <- parameters
                                                        , f <- allDirectFeatureGens
                                                        ]

    let
        allFeatures :: [Feature]
        allFeatures = allFeature1s ++ allFeature2s ++ allDirectFeatures

    forM_ allFeatures $ \ (names, value) ->
        liftIO $ putStrLn $ renderWide $ pretty (mconcat (intersperse "_" names)) <> ":" <+> pretty value


        -- forM (mStatements param) $ \ st ->
        -- case st of
        --     Declaration (Letting nm (Constant value)) -> catMaybes [ i nm value i <- allIntIndicators ]
        --     _ -> return ()
    -- forM_ (mStatements param) $ \ st1 ->
    --     case st1 of
    --         Declaration (Letting nm1 (Constant value1)) ->
    --             case M.lookup nm1 domains of
    --                 Nothing -> return ()
    --                 Just domain1 -> do
    --                     forM_ (mStatements param) $ \ st2 ->
    --                         case st2 of
    --                             Declaration (Letting nm2 (Constant value2)) ->
    --                                 case M.lookup nm2 domains of
    --                                     Nothing -> return ()
    --                                     Just domain2 -> onDomain2 nm1 domain1 value1 nm2 domain2 value2
    --                             _ -> return ()
            -- _ -> return ()




--------------------------------------------------------------------------------
-- types

type Dom = Maybe (Domain () Expression)

type Param = (Name, Dom, Constant)

-- an indicator is some value (like an int itself, the card of a set of the max of the range of a function)
type Indicator = ([Name], Constant)

type IndicatorsGen = Param -> [Indicator]

data FeatureValue = B Bool | I Integer | D Double
    deriving (Eq, Ord, Show)

type Feature = ([Name], FeatureValue)

type FeatureGen1 = [Integer] -> Indicator -> Maybe Feature

-- something like a ratio between two ints
type FeatureGen2 = [Integer] -> Indicator -> Indicator -> Maybe Feature

type DirectFeatureGen = Param -> [Feature]

type DirectFeatureGenM m = Param -> m [Feature]

instance Pretty FeatureValue where
    pretty (B x) = pretty x
    pretty (I x) = pretty x
    pretty (D x) = pretty (printf "%.6f" x :: String)


allIndicatorsGens :: [IndicatorsGen]
allIndicatorsGens = allIntIndicators ++ allBoolIndicators



--------------------------------------------------------------------------------
-- int indicators

allIntIndicators :: [IndicatorsGen]
allIntIndicators = [intValue, cardinality, minMaxOfCollection]

intValue :: IndicatorsGen
intValue (name, _, value@ConstantInt{}) = return ([name, "intValue"], value)
intValue _ = []

cardinality :: IndicatorsGen
cardinality (name, _, ConstantAbstract lit) =
    let len = case lit of
                AbsLitMatrix _  xs -> Just (genericLength xs)
                AbsLitSet       xs -> Just (genericLength xs)
                AbsLitMSet      xs -> Just (genericLength xs)
                AbsLitFunction  xs -> Just (genericLength xs)
                AbsLitSequence  xs -> Just (genericLength xs)
                AbsLitRelation  xs -> Just (genericLength xs)
                AbsLitPartition xs -> Just (genericLength xs)
                _ -> Nothing
    in  case len of
            Just l -> return ([name, "cardinality"], ConstantInt TagInt l)
            Nothing -> []
cardinality _ = []

minMaxOfCollection :: IndicatorsGen
minMaxOfCollection (name, _, ConstantAbstract lit) =
    let intss = case lit of
                    AbsLitMatrix _  xs -> [extractAllInts xs]
                    AbsLitSet       xs -> [extractAllInts xs]
                    AbsLitMSet      xs -> [extractAllInts xs]
                    AbsLitFunction  xs -> [extractAllInts (map fst xs), extractAllInts (map snd xs)]
                    AbsLitSequence  xs -> [extractAllInts xs]
                    AbsLitRelation  xss -> map extractAllInts (transpose xss)
                    AbsLitPartition xss -> [extractAllInts (concat xss)]
                    _ -> []
    in  [ ([name, indexS, txt], ConstantInt TagInt (reducer ints))
        | (index, ints) <- zip allNats intss
        , let indexS = Name $ stringToText $ show index
        , not (null ints)
        , (txt, reducer) <- [("min", minimum), ("max", maximum)]
        ] 
minMaxOfCollection _ = []

extractAllInts :: [Constant] -> [Integer]
extractAllInts xs = [ x | ConstantInt _ x <- xs ]


--------------------------------------------------------------------------------
-- bool indicators

allBoolIndicators :: [IndicatorsGen]
allBoolIndicators = [intIsEven, intIsSquare, intIsPrime]

intIsEven :: IndicatorsGen
intIsEven (name, _, ConstantInt _ value) = return ([name, "isEven"], ConstantBool (mod value 2 == 0))
intIsEven _ = []

intIsSquare :: IndicatorsGen
intIsSquare (name, _, ConstantInt _ value) = return ([name, "isSquare"], ConstantBool (mod value 2 == 0))
intIsSquare _ = []

intIsPrime :: IndicatorsGen
intIsPrime (name, _, ConstantInt _ value) = return ([name, "isPrime"], ConstantBool (isPrime value))
intIsPrime _ = []



--------------------------------------------------------------------------------
-- linear, via indicators

allFeatureGen1s :: [FeatureGen1]
allFeatureGen1s = [ valueis, intRatioToMaxInt, intIsOffByOne, intIsRepeated ]

valueis :: FeatureGen1
valueis _ (name, ConstantBool value) = Just (name, B value)
valueis _ (name, ConstantInt _ value) = Just (name, I value)
valueis _ _ = Nothing

intRatioToMaxInt :: FeatureGen1
intRatioToMaxInt allIntValues (name, ConstantInt _ value) =
    Just (name ++ ["ratioToMax"], D (fromIntegral value / fromIntegral (maximum allIntValues)))
intRatioToMaxInt _ _ = Nothing

intIsOffByOne :: FeatureGen1
intIsOffByOne allIntValues (name, ConstantInt _ value) =
    let flag = any (\ v -> abs (value - v) == 1) allIntValues
    in  Just (name ++ ["intIsOffByOne"], B flag)
intIsOffByOne _ _ = Nothing

intIsRepeated :: FeatureGen1
intIsRepeated allIntValues (name, ConstantInt _ value) =
    let flag = sum [1 :: Int | v <- allIntValues, v == value] >= 2
    in  Just (name ++ ["intIsRepeated"], B flag)
intIsRepeated _ _ = Nothing



--------------------------------------------------------------------------------
-- linear, directly on the param

allDirectFeatureGens ::
    EnumerateDomain m =>
    MonadFail m =>
    MonadReader [(Name, Expression)] m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    [DirectFeatureGenM m]
allDirectFeatureGens = [return . correlation, return . collectionStats, density]

correlation :: DirectFeatureGen
correlation (name, _, ConstantAbstract (AbsLitFunction xs)) =
    let nums = [ (fromIntegral a, fromIntegral b) | (ConstantInt _ a, ConstantInt _ b) <- xs ]
    in  if length nums == length xs && length xs > 0
            then return ([name, "correlation"], D $ pearson (Vector.fromList nums))
            else []
correlation _ = []

collectionStats :: DirectFeatureGen
collectionStats (name, _, ConstantAbstract lit) =
    let intss = case lit of
                    AbsLitMatrix _  xs -> [extractAllInts xs]
                    AbsLitSet       xs -> [extractAllInts xs]
                    AbsLitMSet      xs -> [extractAllInts xs]
                    AbsLitFunction  xs -> [extractAllInts (map fst xs), extractAllInts (map snd xs)]
                    AbsLitSequence  xs -> [extractAllInts xs]
                    AbsLitRelation  xss -> map extractAllInts (transpose xss)
                    AbsLitPartition xss -> [extractAllInts (concat xss)]
                    _ -> []
        median v = v Vector.! (Vector.length v `div` 2)
    in  [ ([name, indexS, txt], D (reducer nums))
        | (index, ints) <- zip allNats intss
        , let indexS = Name $ stringToText $ show index
        , not (null ints)
        , let nums = Vector.fromList (map fromIntegral ints)
        , (txt, reducer) <- [ ("median", median)
                            , ("mean", mean)
                            , ("stdDev", stdDev)
                            , ("harmonicMean", harmonicMean)
                            , ("geometricMean", geometricMean)
                            , ("skewness", skewness0)
                            , ("kurtosis", kurtosis0)
                            ]
        ]
collectionStats _ = []

skewness0 :: Base.Vector v Double => v Double -> Double
skewness0 xs = if stdDev xs == 0 then 0 else skewness xs

kurtosis0 :: Base.Vector v Double => v Double -> Double
kurtosis0 xs = if stdDev xs == 0 then 0 else kurtosis xs

density ::
    EnumerateDomain m =>
    MonadFail m =>
    MonadReader [(Name, Expression)] m =>
    NameGen m =>
    (?typeCheckerMode :: TypeCheckerMode) =>
    DirectFeatureGenM m
density (name, Just domain, constant) = do
    sizes <- case (domain, constant) of
        (DomainFunction _ (FunctionAttr _ PartialityAttr_Partial _) innerDomFr _, ConstantAbstract (AbsLitFunction entries)) -> do
            maxNbEntries <- do
                dSizeX :: Expression <- domainSizeOf innerDomFr
                values <- asks id
                ConstantInt _ dSizeC <- instantiateExpression values dSizeX
                return dSizeC
            return $ Just (length entries, maxNbEntries)
        (DomainRelation _ _ innerDoms, ConstantAbstract (AbsLitRelation entries)) -> do
            maxNbEntries <- forM innerDoms $ \ d -> do
                dSizeX :: Expression <- domainSizeOf d
                values <- asks id
                ConstantInt _ dSizeC <- instantiateExpression values dSizeX
                return dSizeC
            return $ Just (length entries, product maxNbEntries)
        _ -> return Nothing
    case sizes of
        Just (nbEntries, maxNbEntries) -> return [([name, "density"], D (fromIntegral nbEntries / fromIntegral maxNbEntries))]
        _ -> return []
density _ = return []



--------------------------------------------------------------------------------
-- quadratic, via indicators

allFeatureGen2s :: [FeatureGen2]
allFeatureGen2s = [intIntRatio]

intIntRatio :: FeatureGen2
intIntRatio _ (nmX, ConstantInt _ x) (nmY, ConstantInt _ y) =
    Just (nmX ++ nmY ++ ["intIntRatio"], D (fromIntegral x / fromIntegral y))
intIntRatio _ _ _ = Nothing



