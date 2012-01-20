{-# LANGUAGE FlexibleContexts #-}

module Phases.QuanRename where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO )
import Control.Monad.State ( MonadState, get, put, evalStateT )
import Data.Generics.Uniplate.Direct ( Uniplate, Biplate, transformBi, descendM, descendBiM )

import Language.Essence

quanRename :: (Applicative m, MonadIO m) => Spec -> m Spec
quanRename s = evalStateT (descendBiM quanVarNaming s)
             $ words "i j k l" ++ map (('q':) . show) [(0::Int)..]

getNextName :: MonadState [String] m => m String
getNextName = do
    (nm:rest) <- get
    put rest
    return nm

quanVarNaming :: (Applicative m, MonadState [String] m) => Expr -> m Expr
quanVarNaming (ExprQuantifier qnName (Identifier qnVar) qnOver qnGuard qnBody) = do
    nextName <- getNextName
    let ren = rename qnVar nextName

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
quanVarNaming x = descendM (encapsulated .quanVarNaming) x

encapsulated :: MonadState s m => m b -> m b
encapsulated comp = do
    st <- get
    result <- comp
    put st
    return result

rename :: Biplate a Expr => String -> String -> a -> a
rename old new x = flip transformBi x $ \ t -> case t of Identifier nm | nm == old -> Identifier new
                                                         _ -> t
