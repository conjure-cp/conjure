{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Representations.Permutation.AsFunction ( permutationAsFunction ) where

-- conjure
import Conjure.Prelude
import Conjure.Bug
import Conjure.Language
import Conjure.Language.DomainSizeOf
import Conjure.Language.Expression.DomainSizeOf ()
import Conjure.Language.ZeroVal ( zeroVal, EnumerateDomain )
import Conjure.Representations.Internal
import Conjure.Representations.Common
import Conjure.Representations.Function.Function1D ( domainValues )

permutationAsFunction :: forall m . (MonadFail m, NameGen m, EnumerateDomain m) => Representation m
permutationAsFunction = Representation chck downD structuralCons downC up
 where
   chck :: TypeOf_ReprCheck m
   chck f (DomainPermutation _ s innerDomain)
       | domainCanIndexMatrix innerDomain
       = map (DomainPermutation Permutation_AsFunction s) <$> f innerDomain 
   chck _ _ = return []

   downD :: TypeOf_DownD m
   downD (name, domain@(DomainPermutation Permutation_AsFunction _ innerDomain))
       | domainCanIndexMatrix innerDomain = do
         --TODO do calc on m and permutation size attribute
       m <- domainSizeOf innerDomain
       return $ Just
           [ ( mkOutName (Just "PermutationFunction") domain name
             , DomainFunction (Function_AsRelation Relation_AsMatrix) (FunctionAttr (SizeAttr_MaxSize m) PartialityAttr_Total JectivityAttr_Bijective) innerDomain innerDomain
             )
           ] 
   downD (_, domain@(DomainPermutation r _ _)) = trace (textToString (representationToShortText r)) $ na "{downD} AsFunction"

   downD _ =  na "{downD} AsFunction"

   structuralCons :: TypeOf_Structural m
   structuralCons f downX1 indom@(DomainPermutation _ _ inner)
     = return $ \p -> do
          refs <- downX1 p
          case refs of
             [f] -> error "partitionAsFunction structuralCons not defined" 

   downC :: TypeOf_DownC m
   downC =  error "partitionAsFunction downC not defined" 

   up :: TypeOf_Up m
   up = error "partitionAsFunction up not defined" 

