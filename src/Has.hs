{-# LANGUAGE FlexibleInstances     #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE ScopedTypeVariables   #-}
{-# LANGUAGE FlexibleContexts      #-}

module Has where

import Control.Monad ( liftM )
import qualified Control.Monad.State as S -- ( MonadState(..), modify )

class Has container field where

    get     :: container -> field

    put     :: field -> container -> container

    modify  :: (field -> field) -> container -> container
    modify f x = put (f (get x)) x

    getM    :: (S.MonadState container m, Has container field) => m field
    getM = get `liftM` S.get

    putM    :: (S.MonadState container m, Has container field) => field -> m ()
    putM = S.modify . put

    modifyM :: (S.MonadState container m, Has container field) => (field -> field) -> m ()
    modifyM = S.modify . modify


instance Has a a where
    get a = a
    put a _ = a
    modify f a = f a

instance Has (a,b) a where
    get (a,_) = a
    put a (_,b) = (a,b)

instance Has (a,b) b where
    get (_,b) = b
    put b (a,_) = (a,b)

instance Has (Either a b) (Maybe a) where
    get (Left  x)  = Just x
    get (Right _)  = Nothing

    put Nothing  c = c
    put (Just x) _ = Left x

instance Has (Either a b) (Maybe b) where
    get (Left  _)  = Nothing
    get (Right x)  = Just x

    put Nothing  c = c
    put (Just x) _ = Right x


-- the following aren't easy to add, I wish instance resolution were clever enough..

-- instance (Has a c) => Has (a,b) c where
--     get (a,_) = get a
--     put c (a,b) = (put c a, b)

-- instance (Has b c) => Has (a,b) c where
--     get (_,b) = get b
--     put c (a,b) = (a,put c b)
