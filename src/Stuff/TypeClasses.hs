module Stuff.TypeClasses where

import Control.Monad ( liftM )
import Control.Monad.State ( MonadState, gets, modify )
import Data.Proxy



class Has a b where
    hasGet :: a -> b
    hasModify :: (b -> b) -> a -> a

instance Has a a where
    hasGet = id
    hasModify = id

hasPut :: Has a b => b -> a -> a
hasPut x = hasModify (const x)

hasGetSt :: (MonadState state m, Has state a) => m a
hasGetSt = gets hasGet

hasPutSt :: (MonadState state m, Has state a) => a -> m ()
hasPutSt x = modify (hasPut x)

hasModifySt :: (MonadState state m, Has state a) => (a -> a) -> m ()
hasModifySt f = do x <- hasGetSt ; hasPutSt (f x)



class Program program statement | program -> statement where
    pStatements :: program -> [statement]
    pUpdateStatements :: [statement] -> program -> program



class Statement statement kind | statement -> kind where
    statementKind :: statement -> kind



class Expression e where
    plate :: e -> ([e], [e] -> e)

children :: Expression e => e -> [e]
children = fst . plate

universe :: Expression e => e -> [e]
universe x = x : concatMap universe (children x)

descendProxy
    :: ( MonadState state m
       , Has state [Binding thing]
       , Expression e
       , IntroduceBindings e
       )
    => Proxy [Binding thing]
    -> (e -> m e) -> e -> m e
descendProxy bindingsP f x = do
    bm <- (`asProxyTypeOf` bindingsP) `liftM` hasGetSt
    introduceBindings x
    let (children, generate) = plate x
    x' <- generate `liftM` mapM f children
    hasPutSt bm
    return x'

descend
    :: forall state m thing e
    .  ( MonadState state m
       , Has state [Binding thing]
       , Expression e
       , IntroduceBindings e
       )
    => (e -> m e) -> e -> m e
descend = descendProxy (Proxy :: Proxy [Binding thing])

transformProxy
    :: ( MonadState state m
       , Has state [Binding thing]
       , Expression e
       , IntroduceBindings e
       )
    => Proxy [Binding thing]
    -> (e -> m e) -> e -> m e
transformProxy bindingsP f x =
    descendProxy bindingsP (transformProxy bindingsP f) x >>= f

transform
    :: forall state m thing e
    .  ( MonadState state m
       , Has state [Binding thing]
       , Expression e
       , IntroduceBindings e
       )
    => (e -> m e) -> e -> m e
transform = transformProxy (Proxy :: Proxy [Binding thing])

rewriteProxy
    :: ( MonadState state m
       , Has state [Binding thing]
       , Expression e
       , IntroduceBindings e
       )
    => Proxy [Binding thing]
    -> (e -> m (Maybe e)) -> e -> m e
rewriteProxy bindingsP f = transformProxy bindingsP g
    where
        g x = f x >>= maybe (return x) (rewriteProxy bindingsP f)

rewrite
    :: forall state m thing e
    .  ( MonadState state m
       , Has state [Binding thing]
       , Expression e
       , IntroduceBindings e
       )
    => (e -> m (Maybe e)) -> e -> m e
rewrite = rewriteProxy (Proxy :: Proxy [Binding thing])

addBinding
    :: ( MonadState state m
       , Has state [Binding thing]
       )
    => String -> thing -> m ()
addBinding nm t = hasModifySt (Binding nm t :)

getAllBoundNames
    :: forall m state thing
    .  ( MonadState state m
       , Has state [Binding thing]
       )
    => m [String]
getAllBoundNames = do
    bm :: [Binding thing] <- hasGetSt
    return (map bindingName bm)

getBinding
    :: ( MonadState state m
       , Has state [Binding thing]
       )
    => String -> m (Maybe thing)
getBinding nm = do
    things :: [Binding thing] <- hasGetSt
    return $ nm `lookup` [ (i,j) | Binding i j <- things ]



data Binding thing = Binding { bindingName  :: String
                             , bindingThing :: thing
                             }



class IntroduceBindings expression where
    introduceBindings
        :: ( Monad m
           -- , BindingsManager thingsManager binding
           -- , Has state (thingsManager binding)
           -- , MonadState state m
           )
        => expression -> m ()



