module Conjure.UI.Split ( outputSplittedModels, removeUnusedDecls ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty

-- pipes
import Pipes ( Producer, yield )
import qualified Pipes.Prelude as Pipes ( foldM )


outputSplittedModels
    :: (MonadIO m, MonadFail m)
    => FilePath
    -> Model
    -> m ()
outputSplittedModels outputDirectory model = do
    liftIO $ createDirectoryIfMissing True outputDirectory
    let
        each i eprime = do
            let gen = padLeft 6 '0' (show i)
            let filename = outputDirectory </> "splitted" ++ gen ++ ".essence"
            liftIO $ writeFile filename (renderNormal eprime)
            return (i+1)
    Pipes.foldM each
                (return (1 :: Int))
                (const $ return ())
                (split model)


split
    :: MonadIO m
    => Model
    -> Producer Model m ()
split m = do
    let upd stmts = m { mStatements = stmts }
    let
        -- Right indicates "declarations", out of these only the needed ones will stay.
        -- Left indicates "other statements", subsets of these will be in the output.
        toPermute st@Declaration{}       = Right st
        toPermute st@SearchOrder{}       = Right st
        toPermute st@SearchHeuristic{}   = Right st
        toPermute (Where xs)             = Left [Where [x] | x <- xs]
        toPermute st@Objective{}         = Left [st]
        toPermute (SuchThat xs)          = Left [SuchThat [x] | x <- xs]
    let (statements, decls) = mStatements m |> map toPermute |> partitionEithers
    forM_ (statements
            |> concat
            |> nub                  -- remove duplicates
            |> subsequences         -- generate all subsequences
            |> tail                 -- drop the first, contains nothing
          ) $ \ stmts ->
        Pipes.yield $ removeUnusedDecls $ upd $ decls ++ stmts
    forM_ (nub decls) $ \ decl ->
        Pipes.yield $ upd [decl]


removeUnusedDecls :: Model -> Model
removeUnusedDecls m = m { mStatements = stmts }
    where
        stmts = concat
            [ case declared st of
                Just nm | nbUses nm after == 0 -> []
                _ -> [st]
            | (st, after) <- withAfter (mStatements m)
            ]

        declared (Declaration (FindOrGiven _ nm _)) = return nm
        declared (Declaration (Letting nm _)) = return nm
        declared (Declaration (GivenDomainDefnEnum nm)) = return nm
        declared (Declaration (LettingDomainDefnEnum nm _)) = return nm
        declared (Declaration (LettingDomainDefnUnnamed nm _)) = return nm
        declared _ = Nothing
