# relation
Contains a list of domains.

Defined by 
``` 
relation (optional attributes) of (domain1 * domain2 * ...)
```

They can be explicitly defined using tuples (of the same type) as
```
letting R be relation((1,1,0),(1,0,1),(0,1,1))
```

Relations have two groups of attributes
1. cardinality related
    - [size](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_size.md)
    - [minSize](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_maxSize.md)
    - [maxSize](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_minSize.md)
2. Binary Relation (only on relations that contain 2 identical domains)
    - [reflexive](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_reflexive.md)
    - [irreflexive](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_irreflexive.md)
    - [coreflexive](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_coreflexive.md)
    - [symmetric](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_symmetric.md)
    - [antiSymmetric](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_antiSymmetric.md)
    - [aSymmetric](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_antiSymmetric.md)
    - [transitive]()
    - [total]()
    - [connex]()
    - [Euclidean]()
    - [serial]()
    - [equivalence]()
    - [partialOrder]()
