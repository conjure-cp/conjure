module Conjure.UI.ParameterGenerator where

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.NameResolution ( resolveNames )

-- | This doesn't do anything to do with correcting categories at the moment, it should.
--   An example:
--      given n : int(1..10)
--      given s : set (size n) of int(1..10)
--   Should output:
--      find n : int(1..10)
--      find s : set (minSize 1, maxSize 10) of int(1..10)
--      such that n = |s|
--   (Just dropping wrong category stuff from attribute list isn't acceptable, because mset.)
parameterGenerator :: (MonadLog m, MonadFail m, MonadUserError m) => Model -> m Model
parameterGenerator = runNameGen . resolveNames >=> core
    where
        core m = do
            outStatements <- forM (mStatements m) $ \ st -> case st of
                Declaration (FindOrGiven Given nm dom) -> do
                    return [Declaration (FindOrGiven Find nm dom)]
                Declaration (FindOrGiven Find  _  _  ) -> return []
                Declaration {}                         -> return [st]
                SearchOrder {}                         -> return []
                Where xs                               -> return [SuchThat xs]
                Objective   {}                         -> return []
                SuchThat    {}                         -> return []
            return m { mStatements = concat outStatements }
