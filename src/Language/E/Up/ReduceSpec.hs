{-# LANGUAGE QuasiQuotes, ViewPatterns, OverloadedStrings  #-}

module Language.E.Up.ReduceSpec(
     reduceSpec
    ,simSpec
    ,removeNegatives
    ,removeIndexRanges
    ,introduceParams
    ,introduceParams'
    ,toLst
    ,inlineSpec
    ,specSimplify
) where

import Language.E
import Language.E.Evaluator.Full    ( fullEvaluator)
import Language.E.Up.Debug
import Language.E.Pipeline.InlineLettings(inlineLettings)

-- Convert the unaryOp.negate.value.literal to a value.literal
removeNegatives :: Monad m => Spec -> m Spec
removeNegatives spec@(Spec _ _) = do
    let (s,_) = head $ removeNegatives' spec
        ee    = (head .rights) [s]

    return  ee

    where 

    removeNegatives' ::  Spec -> [(Either Doc Spec, LogTree)]
    removeNegatives' spec1 = runCompE "removeNegatives'" $ bottomUpSpec' convertNegatives spec1

    convertNegatives :: MonadConjure m => E -> m E
    convertNegatives [xMatch| [Prim (I n )] := unaryOp.negate.value.literal|] = 
        return $ [xMake|  value.literal := [Prim (I (- n) )] |]

    convertNegatives x = return x

removeIndexRanges :: Monad m => Spec -> m Spec
removeIndexRanges spec@(Spec _ _) = do
    let (s,_) = head $ removeIndexRanges' spec
        ee    = (head .rights) [s]

    return  ee

    where 

    removeIndexRanges' ::  Spec -> [(Either Doc Spec, LogTree)]
    removeIndexRanges' spec1 = runCompE "removeIndexRanges'" $ bottomUpSpec' removeIndexRange spec1

    removeIndexRange :: MonadConjure m => E -> m E
    removeIndexRange [xMatch| v  := matrix.values
                            | _  := matrix.indexrange|] =
        return [xMake| matrix.values := v |]

    removeIndexRange x = return x


introduceParams :: Monad m => Maybe (m Spec) -> Spec -> m Spec
introduceParams (Just par) spec@(Spec ver _) = do
    param <- par
    let (pe,ee) = (toLst param, toLst spec)
        ef = pe ++ filter removeGiven  ee

    return $  Spec ver (listAsStatement ef)

introduceParams Nothing spec@(Spec ver _) = do
    let ef = filter removeGiven (toLst spec)
    return $  Spec ver (listAsStatement ef)

introduceParams' :: Monad m => Maybe Spec -> Spec -> m Spec
introduceParams' (Just param) spec@(Spec ver _) = do
    let (pe,ee) = (toLst param, toLst spec)
        ef = pe ++ filter removeGiven  ee

    return $  Spec ver (listAsStatement ef)

introduceParams' Nothing spec@(Spec ver _) = do
    let ef = filter removeGiven (toLst spec)
    return $  Spec ver (listAsStatement ef)

reduceSpec  :: Monad m => Spec -> m Spec
reduceSpec  spec = 
    return $ inlineSpec spec


simSpec :: Monad m => Spec -> m Spec
simSpec spec@(Spec _ _) = do
    -- this part is very slow  but 
    -- handles test cases such as expr
    let (s',_) = head' $ re spec
    return $ case  rights [s'] of 
               [] -> spec
               x  -> head' x
    where
    re spec1 = runCompE "sim" $ bottomUpSpecExcept' excludeSim sim spec1

sim :: MonadConjure m => E -> m E
sim x = do
    res <- fullEvaluator x
    case res of
        Just (e,_) -> return e
        Nothing    -> return x

excludeSim :: E -> Bool
excludeSim _ = False 

head'        :: [a] -> a
head' (x:_)  =  x
head' []     =  _bugg "head: simSpec"

toLst :: Spec -> [E]
toLst (Spec _ ee) = statementAsList ee


removeGiven :: E -> Bool
removeGiven [xMatch| _ := topLevel.declaration.given |] = False
removeGiven _ = True

inlineSpec :: Spec -> Spec
inlineSpec spec =
    let
        (mresult, _logs) = runCompESingle "inlining lettings" helper
    in
        case mresult of
            Left  x      -> error $ renderNormal x
            Right result -> result

    where
    helper :: FunkySingle ConjureState ConjureError Identity Spec
    helper = do
        let pipeline = recordSpec "init" 
                >=> inlineLettings >=> recordSpec "inlineLettings"
        pipeline spec

specSimplify :: Spec -> Spec
specSimplify spec =
    let
        (mresult, _logs) = runCompESingle "simplifySpec" helper
    in
        case mresult of
            Left  x      -> error $ renderNormal x
            Right result -> result

    where
    helper :: FunkySingle ConjureState ConjureError Identity Spec
    helper = do
        let pipeline = recordSpec "init" 
                >=> simplifySpec >=> recordSpec "simplifySpec"
        pipeline spec

-- bd :: MonadConjure m => E -> m E
-- bd x = do
    -- bs <- bindersDoc'
    -- mkLog "Bindings For"  (vcat [pretty x, bs])
    -- return x


-- bindersDoc' :: MonadConjure m => m Doc
-- bindersDoc' = do
    -- bs <- gets binders
    -- let bs' = nubBy ((==) `on` binderName) bs
    -- return $ vcat [ pretty nm <+> ":" <+> pretty val | Binder nm val <- bs' ]

_bug :: String -> [E] -> t
_bug  s = upBug  ("ReduceSpec: " ++ s)
_bugi :: (Show a) => String -> (a, [E]) -> t
_bugi s = upBugi ("ReduceSpec: " ++ s )
_bugg :: String -> t
_bugg s = _bug s []

