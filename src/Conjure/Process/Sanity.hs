module Conjure.Process.Sanity ( sanityChecks ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty


sanityChecks :: MonadFail m => Model -> m Model
sanityChecks m = do

    forM_ (mStatements m) $ \ st -> case st of
        Declaration (FindOrGiven _ _ domain) ->
            case domain of
                DomainMSet _ (MSetAttr size occur) _ ->
                    case (size, occur) of
                        (SizeAttr_Size{}, _) -> return ()
                        (SizeAttr_MaxSize{}, _) -> return ()
                        (SizeAttr_MinMaxSize{}, _) -> return ()
                        (_, OccurAttr_MaxOccur{}) -> return ()
                        (_, OccurAttr_MinMaxOccur{}) -> return ()
                        _ -> fail $ vcat
                            [ "Error: mset requires (at least) one of the following attributes: size, maxSize, maxOccur"
                            , "When working on:" <++> pretty st
                            ]
                _ -> return ()
        _ -> return ()

    return m
