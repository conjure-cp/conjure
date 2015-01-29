
abstractTypes :: [String]
abstractTypes = [ "set", "mset", "function", "relation", "partition"]

allAttributes :: [(String,String)]
allAttributes = [ (t, a, "int") | t <- abstractTypes, a <- cardinalityAttributes ]
             ++ [ ("mset", "minOccur", "int"), ("mset", "maxOccur", "int") ]
             ++ [ ("function", "total", "()"), ("function", "injective", "()"), ("function", "surjective", "()"), ("function", "bijective", "()") ]
             ++ [ ("relation", "reflexive"     , "()")      -- forAll i : dom . rel(i,i)                ---- where dom is the domain of the columns. each column needs to have the same domain. can work on n-ary.
                , ("relation", "irreflexive"   , "()")      -- forAll i : dom . !rel(i,i)
                , ("relation", "coreflexive"   , "()")      -- forAll i,j : dom . rel(i,j) -> i = j     ---- but i=j does not imply rel(i,j)
                , ("relation", "symmetric"     , "()")      -- forAll (i,j) in rel . rel(j,i)
                , ("relation", "antisymmetric" , "()")      -- forAll i,j : dom . rel(i,j) /\ rel(j,i) -> i = j
                , ("relation", "asymmetric"    , "()")      -- forAll (i,j) in rel . !rel(j,i)
                , ("relation", "transitive"    , "()")      -- forAll (i,j) in toSet(r) . forAll tuple (k) in toSet(r(j,_)) . r(i,k)    ---- defined only on binary relations.
                , ("relation", "total"         , "()")      -- forAll i,j : dom . rel(i,j) \/ rel(j,i)
                , ("relation", "Euclidean"     , "()")      -- rel(i,k) /\ rel(j,k) -> rel(i,j) /\ rel(j,i)                             ---- seems to imply symmetric too.
                , ("relation", "serial"        , "()")      -- forAll i : dom . exists j : dom . rel(i,j)
                , ("relation", "equivalence"   , "()")      -- reflexive, symmetric, and transitive
                , ("relation", "partial-order" , "()")      -- reflexive, antisymmetric, and transitive
                -- there are more...
                -- see http://en.wikipedia.org/wiki/Binary_relation
                ]
             ++ [ ("partition", "numParts", "int"), ("partition", "minNumParts", "int"), ("partition", "maxNumParts", "int")
                , ("partition", "partSize", "int"), ("partition", "minPartSize", "int"), ("partition", "maxPartSize", "int")
                , ("partition", "regular", "()"), ("partition", "complete", "()")
               ]
    where
        cardinalityAttributes = [ "size", "minSize", "maxSize" ]

