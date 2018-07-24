{-# LANGUAGE QuasiQuotes #-}

module Conjure.Process.DealWithCuts ( dealWithCuts ) where

import Conjure.Prelude
import Conjure.UserError
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.TH
import Conjure.Language.ModelStats ( finds )
import Conjure.Language.Pretty


dealWithCuts
    :: (MonadFail m, NameGen m, MonadUserError m)
    => Model
    -> m Model
dealWithCuts m = do
    checkMultiple m
    statements <- forM (mStatements m) $ \ statement ->
        case statement of
            SearchOrder orders0 -> do
                (orders1, (newVars, newCons)) <- runWriterT $ forM orders0 $ \ order ->
                    case order of
                        Cut x -> do
                            varName <- nextName "cut"
                            let varDecl = Declaration (FindOrGiven Find varName DomainBool)
                            let varRef  = Reference varName (Just (DeclNoRepr Find varName DomainBool NoRegion))
                            tell ( [ varDecl ]
                                 , [ [essence| !&varRef <-> &x |] ]
                                 )
                            return $ BranchingOn varName
                        _ -> return order
                orders2 <-
                        if null [ () | BranchingOn{} <- orders0 ]     -- no variables were given, all assumed non-aux
                            then return [ BranchingOn nm | (nm, _) <- finds m ]
                            else return []
                return $ concat
                    [ newVars
                    , [SearchOrder (orders1++orders2)]
                    , [SuchThat newCons]
                    ]
            _ -> return [statement]
    return m { mStatements = concat statements }


checkMultiple :: MonadUserError m => Model -> m ()
checkMultiple m = do
    let searchOrders = [ st | st@SearchOrder{}     <- mStatements m ]
    let heuristics   = [ st | st@SearchHeuristic{} <- mStatements m ]
    let msgs = concat
            [ [ "Expected at most one 'branching on' statement, but got:"
                    <++> vcat (map pretty searchOrders)
              | length searchOrders > 1
              ]
            , [ "Expected at most one 'heuristic' statement, but got:"
                    <++> vcat (map pretty heuristics)
              | length heuristics > 1
              ]
            ]
    when (length msgs > 0) $ userErr msgs


