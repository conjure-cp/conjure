module Conjure.UI.Split ( outputSplittedModels ) where

import Conjure.Prelude
import Conjure.Language.Definition
import Conjure.Language.Pretty
import Conjure.UI.Model ( nbUses )

-- pipes
import Pipes ( Producer, yield )
import qualified Pipes.Prelude as Pipes ( foldM )


outputSplittedModels
    :: (MonadIO m, MonadFail m, MonadLog m)
    => FilePath
    -> Model
    -> m ()
outputSplittedModels outputDirectory model = do
    liftIO $ createDirectoryIfMissing True outputDirectory
    let
        each i eprime = do
            let gen = paddedNum i
            let filename = outputDirectory </> "splitted" ++ gen ++ ".essence"
            liftIO $ writeFile filename (renderNormal eprime)
            return (i+1)
    Pipes.foldM each
                (return (1 :: Int))
                (const $ return ())
                (split model)


split
    :: (MonadIO m, MonadFail m)
    => Model
    -> Producer Model m ()
split m = do
    let toPermute Declaration{} = False
        toPermute SearchOrder{} = False
        toPermute Where{}       = True
        toPermute Objective{}   = True
        toPermute SuchThat{}    = True
    let (decls, statements) = partition toPermute (mStatements m)
    forM_ (subsequences statements |> tail          -- drop the first, contains nothing
                                   |> init          -- drop the last,  contains everything
                                   ) $ \ stmts -> do
        let m' = m { mStatements = decls ++ stmts }
        Pipes.yield (removeUnusedDecls m')
    forM_ decls $ \ decl -> do
        Pipes.yield m { mStatements = [decl] }


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
