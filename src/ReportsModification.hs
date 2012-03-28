module ReportsModification ( ReportsModification, modified, runModifT ) where

import Control.Monad.State ( StateT, runStateT, put )

type ReportsModification m a = StateT Bool m a

modified :: Monad m => ReportsModification m ()
modified = put True

runModifT :: Monad m => ReportsModification m a -> m (Maybe a)
runModifT m = do
    (res,b) <- runStateT m False
    if b
        then return $ Just res
        else return $ Nothing
