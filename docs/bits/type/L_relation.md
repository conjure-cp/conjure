# relation

A list of pairs of elements of given domains that are related to each other.

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
    - [aSymmetric](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_aSymmetric.md)
    - [transitive](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_transitive.md)
    - [total](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_total.md)
    - [connex](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_connex.md)
    - [Euclidean](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_Euclidean.md)
    - [serial](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_serial.md)
    - [equivalence](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_equivalence.md)
    - [partialOrder](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_partialOrder.md)

See this demonstrated [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/RelationDomains.ipynb).
