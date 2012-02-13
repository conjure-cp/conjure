{-# LANGUAGE FlexibleContexts #-}

module Phases.TupleExplode where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO(..) )
import Data.Generics.Uniplate.Direct ( transformBiM )
import Control.Monad.Writer ( MonadWriter, tell, runWriterT )
import Data.Monoid ( Any(..) )

import Language.Essence
import Language.EssencePrinters
import PrintUtils


tupleExplode :: (Applicative m, MonadIO m) => Spec -> m Spec
tupleExplode spec = do
    (spec',Any b) <- doOnce spec
    -- return spec'
    if b
        then tupleExplode spec'
        else return spec'
    where
        -- doOnce :: Spec -> m (Spec,Any)
        doOnce s = runWriterT $ f (topLevelBindings s) s {topLevelBindings=[]}

        -- f :: [Binding] -> Spec -> m Spec
        f []     s = return s
        f (t:ts) s = onOne t s >>= f ts


onOne :: (Applicative m, MonadWriter Any m, MonadIO m) => Binding -> Spec -> m Spec

-- unary tuple at top level
onOne (bEnum,bName,DomainTuple{components=[c]})
    = fmap (addDeclarations [(bEnum,bName,c)])
    . transformBiM f
    where
        f (GenericNode Index [Identifier nm,ValueInteger 0])
            | nm == bName
            = rreturn $ Identifier bName
        f x = return x

-- tuple at top level
onOne (bEnum,bName,DomainTuple{components=cs})
    = fmap (addDeclarations [ (bEnum,bName++"_t"++show i,c) | (i,c) <- zip [(0::Int)..] cs ])
    . transformBiM f
    where
        f (GenericNode Index [Identifier nm,ValueInteger i])
            | nm == bName
            = rreturn $ Identifier $ bName ++ "_t" ++ show i
        f x = return x

-- -- matrix of unary tuples
-- onOne (bEnum,bName,d@DomainMatrix{element=DomainTuple{components=[c]}})
--     = fmap (addDeclarations [(bEnum,bName,d{element=c})])
--     . transformBiM f
--     where
--         f (GenericNode Index [ GenericNode Index [Identifier nm,i]
--                              , ValueInteger 0
--                              ])
--             | nm == bName
--             = rreturn $ GenericNode Index [Identifier nm,i]
--         f x = return x
-- 
-- -- matrix of tuples
-- onOne (bEnum,bName,d@DomainMatrix{element=DomainTuple{components=cs}})
--     = fmap (addDeclarations [(bEnum,bName,DomainTuple { components = [ d { element = c } | c <- cs ]
--                                                       , representation = Nothing
--                                                       })
--                             ])
--     . transformBiM f
--     where
--         f :: MonadWriter Any m => Expr -> m Expr
--         f (GenericNode Index [ GenericNode Index [Identifier nm,i]
--                              , j
--                              ])
--             | nm == bName
--             = rreturn $ GenericNode Index [ GenericNode Index [Identifier nm,j]
--                                           , i
--                                           ]
--         f x = return x

onOne theBinding@(bEnum,bName,dom) = do
    let
        collectIndicesDom :: Expr -> ([Expr],Expr)
        collectIndicesDom (DomainMatrix ind elm) = let t = collectIndicesDom elm in (ind:fst t, snd t)
        collectIndicesDom t = ([], t)

        fromIndicesDom :: [Expr] -> Expr -> Expr
        fromIndicesDom []     t = t
        fromIndicesDom (i:is) t = DomainMatrix i (fromIndicesDom is t)

        collectIndices :: Int -> Expr -> Maybe (Expr,[Expr])
        collectIndices  0 x = Just (x,[])
        collectIndices  n (GenericNode Index [m,i]) = do t <- collectIndices (n-1) m; return (fst t, i:snd t)
        collectIndices _n _x = Nothing -- error $ "collectIndices: " ++ show n ++ " " ++ render prExpr x

        fromIndices :: Expr -> [Expr] -> Expr
        fromIndices m []     = error $ "fromIndices: " ++ render prExpr m
        fromIndices m [i]    = GenericNode Index [m,i]
        fromIndices m (i:is) = fromIndices (GenericNode Index [m,i]) is

        (indices,tau) = collectIndicesDom dom

        f :: (MonadWriter Any m, MonadIO m) => Expr -> m Expr
        f x = case collectIndices ((length indices)+1) x of
                Just (_,[]) -> return x
                Just (Identifier nm,js) | nm == bName -> do
                    let is = head js : reverse (tail js)
                    x' <- rreturn $ fromIndices (Identifier nm) is
                    -- liftIO $ do
                    --     putStrLn $ "f: " ++ nm
                    --     putStrLn $ "   " ++ show is
                    --     putStrLn $ "   " ++ render prExpr x'
                    return x'
                _ -> return x

    -- error $ render prExpr $ indexInto (Identifier "m") [ValueInteger i | i <- [2..6] ]

    let noop = fmap (addDeclarations [theBinding]) . return

    case (indices,tau) of
        ([],_) -> noop
        (_,DomainTuple cs _) ->
            fmap (addDeclarations [(bEnum,bName,DomainTuple { components = [ fromIndicesDom indices c | c <- cs ]
                                                            , representation = Nothing
                                                            }
                                  )]
                 ) . transformBiM f
        _ -> noop

-- onOne x = error $ "Phases.TupleExplode.onOne " ++ show x


rreturn :: MonadWriter Any m => a -> m a
rreturn x = do
    tell $ Any True
    return x
