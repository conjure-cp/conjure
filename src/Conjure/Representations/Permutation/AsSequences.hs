{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE ViewPatterns #-}

module Conjure.Representations.Permutation.AsSequences ( permutationAsSequences ) where

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

permutationAsSequences :: forall m . (MonadFail m, NameGen m, EnumerateDomain m) => Representation m
permutationAsSequences = Representation chck downD structuralCons downC up
 where
   chck :: TypeOf_ReprCheck m
   chck f (DomainPermutation _ s innerDomain)
       | domainCanIndexMatrix innerDomain
       = map (DomainPermutation Permutation_AsSequences s) <$> f innerDomain 
   chck _ _ = return []

   downD :: TypeOf_DownD m
   downD (name, domain@(DomainPermutation Permutation_AsSequences _ innerDomain))
       | domainCanIndexMatrix innerDomain = do
         --TODO do calc on m and permutation size attribute
       m <- domainSizeOf innerDomain
       return $ Just
           [ ( mkOutName (Just "SetOfSequences") domain name
             , DomainSet Set_ExplicitVarSizeWithFlags (SetAttr (SizeAttr_MaxSize m)) 
                   (DomainSequence Sequence_ExplicitBounded
                         (SequenceAttr (SizeAttr_MaxSize m) JectivityAttr_None) innerDomain) 
             )
           ] 
   downD (_, domain@(DomainPermutation r _ _)) = trace (textToString (representationToShortText r)) $ na "{downD} AsSequences"

   downD _ =  na "{downD} AsSequences"

   structuralCons :: TypeOf_Structural m
   structuralCons f downX1 indom@(DomainPermutation _ _ inner)
     = return $ \p -> do
          refs <- downX1 p
          case refsof
             [set] -> do
                outDomSet <- outDomainSet inDom
                innerStructuralConsGenSet <- f outDomSet
                --define quantifieds
                concat <$> sequence
                    [ innerStructuralConsGenSet set 
                    , return [[essence|
                        forAll (&seqAPat, &seqBPat) in set .
                          forAll &i : inner .
                            &i in &seqAPat <-> !(&i in &seqBPat)
                              |]
                             ]
                    ]

   downC :: TypeOf_DownC m
   downC =  error "partitionAsSequence downC not defined" 

   up :: TypeOf_Up m
   up = error "partitionAsSequence up not defined" 

