{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE RankNTypes          #-}

module GenericOps.GMatch where

import Control.Monad ( zipWithM_ )
import Control.Monad.State ( MonadState, gets, modify )
import Control.Monad.Error ( MonadError, throwError )
import Control.Arrow ( first, second )

import GenericOps.Core


gmatchError ::
    ( MonadState ( [(String,GNode)]
                 , [(GNode,GNode)]
                 ) m
    , MonadError String m
    ) => String -> m ()
gmatchError msg = do
    stack <- gets snd
    let msgs = map (\ (a,b) -> showG a ++ " ~~ " ++ showG b ) stack
    let combinedMsg = unlines (msg : map ("\t"++) msgs)
    throwError combinedMsg

gmatch ::
    ( MonadState ( [(String,GNode)]
                 , [(GNode,GNode)]
                 ) m
    , MonadError String m
    ) => GNode -> GNode -> m ()
gmatch pattern@(GNode ty_p p) actual@(GNode ty_a a) = do
    modify $ second ((pattern,actual) :)                                                                -- add this node on top of the call stack.
    case hole p of
        UnnamedHole  -> return ()                                                                       -- unnamed hole: matching succeeds, no bindings.
        NamedHole nm -> modify $ first ((nm,actual) :)                                                  -- named hole: matching succeeds, bind the name to rhs.
        NotAHole     | ty_p /= ty_a -> gmatchError "Type mismatch"                                      -- types must match.
                     | otherwise    -> do
            let children_p = fst $ gplate p
            let children_a = fst $ gplate a
            case (children_p, children_a) of
                ([], []) | pattern === actual -> return ()                                              -- if this is a leaf, must check for equality.
                         | otherwise          -> gmatchError "Leafs inequal."                           -- otherwise matching fails.
                (ps, as) -> case () of
                              _ | nodeTag p /= nodeTag a -> gmatchError "Constructor mismatch."         -- node tags must match.
                                | length ps /= length as -> gmatchError "Shape mismatch."               -- with equal number of children.
                                | otherwise -> zipWithM_ gmatch ps as                                   -- recursive call.
    modify $ second tail

gmatchCall ::
    ( MonadState ( [(String,GNode)]
                 , [(GNode,GNode)]
                 ) m
    , MonadError String m
    ) => GNode -> GNode -> m [(String,GNode)]
gmatchCall p a = do gmatch p a; gets fst



gbindError ::
    ( MonadState ( [(String,GNode)]
                 , [GNode]
                 ) m
    , MonadError String m
    ) => String -> m GNode
gbindError msg = do
    stack <- gets snd
    let msgs = map showG stack
    let combinedMsg = unlines (msg : map ("\t"++) msgs)
    throwError combinedMsg

gbind ::
    ( MonadState ( [(String,GNode)]
                 , [GNode]
                 ) m
    , MonadError String m
    ) => GNode -> m GNode
gbind template@(GNode ty_t t) = do
    modify $ second (template :)                                                                                    -- add this node on top of the call stack.
    case hole t of
        UnnamedHole  -> gbindError "Unnamed hole in template"                                                       -- unnamed hole in a template is just nonsense.
        NamedHole nm -> do
            bindings <- gets fst
            case lookup nm bindings of
                Nothing -> gbindError ("Not found: " ++ nm)                                                         -- if the name cannot be found in te list of bindings.
                Just bound@(GNode ty_b _) | ty_t == ty_b -> return bound                                            -- the name is bound to something of the expected type. great.
                                          | otherwise    -> gbindError $ "Type mismatch for: " ++ nm ++ "\n"        -- name is bound, but wrong type.
                                                                      ++ "\tExpected: " ++ show ty_t ++ "\n"
                                                                      ++ "\tBut got:  " ++ show ty_b
        NotAHole -> case gplate t of
                        ([], _  ) -> return template                                                                -- if this is not a hole, and is a leaf, just return it.
                        (ts, gen) -> do
                            ts' <- mapM gbind ts                                                                    -- otherwise, apply gbind recursively to immediate children.
                            return $ mkG $ gen ts'                                                                  -- and construct the result using the new children.

