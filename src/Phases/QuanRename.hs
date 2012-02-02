{-# LANGUAGE FlexibleContexts     #-}
{-# LANGUAGE FlexibleInstances    #-}

module Phases.QuanRename ( quanRenameSt, quanRenameIO ) where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO(..) )
import Control.Monad.State ( MonadState(..), StateT, evalStateT )
import Data.Generics.Uniplate.Direct ( Uniplate, Biplate, transformBi, descendM, descendBiM )
import Data.List ( (\\) )

import Language.Essence
import UniqueSupply
import Utils ( snd3 )


quanRenameSt :: (Applicative m, MonadIO m) => Spec -> m Spec
quanRenameSt spec = do
    let allNames = words "i j k l" ++ map (("q_"++) . show) [(0::Int)..]
    let names    = allNames \\ map snd3 (topLevelBindings spec)
    evalStateT (descendBiM (encapsulated . quanVarNaming) spec) names

quanRenameIO :: Biplate a Expr => a -> IO a
quanRenameIO sp = descendBiM (encapsulated . quanVarNaming) sp


quanVarNaming :: (RenamingMonad m) => Expr -> m Expr
quanVarNaming p@(ExprQuantifier qnName (Identifier qnVar) qnOver qnGuard qnBody) = do
    b <- shouldRename qnVar
    if not b
        then return p
        else do
            nextName <- getNextName
            let
                rename :: Biplate a Expr => String -> String -> a -> a
                rename old new x = flip transformBi x $ \ t ->
                    case t of
                        Identifier nm | nm == old -> Identifier new
                        _ -> t
                ren = rename qnVar nextName

            qnOver'  <- encapsulated $ quanVarNaming (ren qnOver)

            qnGuard' <- encapsulated $ case qnGuard of
                                            Nothing -> return Nothing
                                            Just t  -> Just <$> quanVarNaming (ren t)

            qnBody'  <- encapsulated $ quanVarNaming (ren qnBody)

            return $ ExprQuantifier qnName
                                    (Identifier nextName)
                                    qnOver'
                                    qnGuard'
                                    qnBody'
quanVarNaming x = descendM (encapsulated . quanVarNaming) x


class (Applicative m, Monad m) => RenamingMonad m where
    getNextName :: m String

    encapsulated :: m a -> m a
    encapsulated = id

    shouldRename :: String -> m Bool
    shouldRename _ = return True

instance (Applicative m, MonadIO m) => RenamingMonad (StateT [String] m) where
    getNextName = do
        (nm:rest) <- get
        put rest
        -- liftIO . putStrLn $ "generated new name: " ++ nm
        return nm
    encapsulated comp = do
        st <- get
        result <- comp
        put st
        return result

instance RenamingMonad IO where
    getNextName = do
        i <- nextUniqueInt
        let nm = "UQ_" ++ show i
        -- putStrLn $ "generated new name: " ++ nm
        return nm
    -- encapsulated comp = do
    --     st <- readIORef intCounter
    --     result <- comp
    --     writeIORef intCounter st
    --     return result
    shouldRename ('U':'Q':'_':_) = return False
    shouldRename _ = return True
