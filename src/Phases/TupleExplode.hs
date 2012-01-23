{-# LANGUAGE FlexibleContexts #-}

module Phases.TupleExplode where

import Control.Applicative
import Control.Monad.IO.Class ( MonadIO )
import Data.Generics.Uniplate.Direct ( transformBiM )
import Control.Monad.Writer ( MonadWriter, tell, runWriterT )
import Data.Monoid ( Any(..) )
import Data.List ( (\\) )

import Language.Essence


tupleExplode :: (Applicative m, MonadIO m) => Spec -> m Spec
tupleExplode spec = do
    let tuples = [ b | b@(_,_,DomainTuple{})
                     <- topLevelBindings spec
                     ]
              ++ [ b | b@(_,_,DomainMatrix{element=DomainTuple{}})
                     <- topLevelBindings spec
                     ]
    (spec',Any b) <- runWriterT $ f tuples $ spec { topLevelBindings = topLevelBindings spec \\ tuples }
    if b
        then tupleExplode spec'
        else return spec'
    where
        f []     s = return s
        f (t:ts) s = onOne t s >>= f ts

onOne :: (Applicative m, MonadWriter Any m) => Binding -> Spec -> m Spec

onOne (bEnum,bName,DomainTuple{components=[c]})
    = fmap (addDeclarations [(bEnum,bName,c)])
    . transformBiM f
    where
        f (GenericNode Index [Identifier nm,ValueInteger 0])
            | nm == bName
            = rreturn $ Identifier bName
        f x = return x
onOne (bEnum,bName,DomainTuple{components=cs})
    = fmap (addDeclarations [ (bEnum,bName++"_"++show i,c) | (i,c) <- zip [(0::Int)..] cs ])
    . transformBiM f
    where
        f (GenericNode Index [Identifier nm,ValueInteger i])
            | nm == bName
            = rreturn $ Identifier $ bName ++ "_" ++ show i
        f x = return x

onOne (bEnum,bName,d@DomainMatrix{element=DomainTuple{components=[c]}})
    = fmap (addDeclarations [(bEnum,bName,d{element=c})])
    . transformBiM f
    where
        f (GenericNode Index [ GenericNode Index [Identifier nm,i]
                             , ValueInteger 0
                             ])
            | nm == bName
            = rreturn $ GenericNode Index [Identifier nm,i]
        f x = return x
onOne (bEnum,bName,d@DomainMatrix{element=DomainTuple{components=cs}})
    = fmap (addDeclarations [(bEnum,bName,DomainTuple { components = [ d { element = c } | c <- cs ]
                                                      , representation = Nothing
                                                      })
                            ])
    . transformBiM f
    where
        f (GenericNode Index [ GenericNode Index [Identifier nm,i]
                             , j
                             ])
            | nm == bName
            = rreturn $ GenericNode Index [ GenericNode Index [Identifier nm,j]
                                          , i
                                          ]
        f x = return x


onOne x = error $ "Phases.TupleExplode.onOne " ++ show x


rreturn :: MonadWriter Any m => a -> m a
rreturn x = do
    tell $ Any True
    return x
