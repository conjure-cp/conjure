module Conjure.Language.ModelStats
    ( givens, nbGivens, nbAbstractGivens
    , finds, nbFinds, nbAbstractFinds
    , declarations, nbDeclarations, nbAbstractDeclarations
    , lettings
    , domainNeedsRepresentation
    , modelInfo
    ) where

import Conjure.Prelude
import Conjure.Bug
import Conjure.Language.Definition
import Conjure.Language.Domain
import Conjure.Language.Pretty


givens :: Model -> [(Name, Domain () Expression)]
givens m = [ (nm,d) | Declaration (FindOrGiven Given nm d) <- mStatements m ]

nbGivens :: Model -> Int
nbGivens = length . givens

nbAbstractGivens :: Model -> Int
nbAbstractGivens = length . filter domainNeedsRepresentation . map snd . givens


finds :: Model -> [(Name, Domain () Expression)]
finds m = [ (nm,d) | Declaration (FindOrGiven Find nm d) <- mStatements m ]

nbFinds :: Model -> Int
nbFinds = length . finds

nbAbstractFinds :: Model -> Int
nbAbstractFinds = length . filter domainNeedsRepresentation . map snd . finds


declarations :: Model -> [(Name, Domain () Expression)]
declarations m = [ (nm,d) | Declaration (FindOrGiven _ nm d) <- mStatements m ]

nbDeclarations :: Model -> Int
nbDeclarations = length . declarations

nbAbstractDeclarations :: Model -> Int
nbAbstractDeclarations = length . filter domainNeedsRepresentation . map snd . declarations


lettings :: Model -> [(Name, Expression)]
lettings m = [ (nm,x) | Declaration (Letting nm x) <- mStatements m ]


domainNeedsRepresentation :: (Pretty r, Pretty x) => Domain r x -> Bool
domainNeedsRepresentation DomainBool{} = False
domainNeedsRepresentation DomainInt{} = False
domainNeedsRepresentation DomainEnum{} = False
domainNeedsRepresentation DomainUnnamed{} = False
domainNeedsRepresentation DomainTuple{} = False
domainNeedsRepresentation (DomainMatrix _ inner) = domainNeedsRepresentation inner
domainNeedsRepresentation DomainSet{}       = True
domainNeedsRepresentation DomainMSet{}      = True
domainNeedsRepresentation DomainFunction{}  = True
domainNeedsRepresentation DomainRelation{}  = True
domainNeedsRepresentation DomainPartition{} = True
domainNeedsRepresentation d = bug $ "domainNeedsRepresentation:" <+> pretty d


modelInfo :: Model -> Doc
modelInfo m = vcat
    [ "Contains" <+> pretty   (nbGivens m) <+> "parameters        "
                 <+> prParens (pretty (nbAbstractGivens m) <+> "abstract")
    , "        " <+> pretty   (nbFinds  m) <+> "decision variables"
                 <+> prParens (pretty (nbAbstractFinds m ) <+> "abstract")
    ]

