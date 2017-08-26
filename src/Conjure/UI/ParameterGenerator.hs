module Conjure.UI.ParameterGenerator where

import Conjure.Prelude
import Conjure.Language
import Conjure.Language.NameResolution ( resolveNames )
import Conjure.Language.Expression.DomainSizeOf ( domainSizeOf )


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
parameterGenerator model = runNameGen model (resolveNames model) >>= core
    where
        core m = do
            (outStatements, errs) <- runWriterT $ forM (mStatements m) $ \ st -> case st of
                Declaration (FindOrGiven Given nm dom) ->
                    case domainSizeOf dom of
                        Nothing -> tell [(nm, dom)] >> return []
                        Just (_ :: Expression)
                                -> return [Declaration (FindOrGiven Find nm dom)]
                Declaration (FindOrGiven Find  _  _  ) -> return []
                Declaration       {}                   -> return [st]
                SearchOrder       {}                   -> return []
                SearchHeuristic   {}                   -> return []
                Where             xs                   -> return [SuchThat xs]
                Objective         {}                   -> return []
                SuchThat          {}                   -> return []
                SNS_Group{}                            -> return []
                SNS_Neighbourhood{}                    -> return []
                SNS_Out_Neighbourhood{}                -> return []
                SNS_Out_IncumbentMapping{}             -> return []
            if null errs
                then return m { mStatements = concat outStatements }
                else userErr1 $ vcat $ "Some parameters have infinite domains."
                                     : [ pretty nm <> ":" <++> pretty dom
                                     | (nm, dom) <- errs
                                     ]
