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
    - [size]()
    - [minSize]()
    - [maxSize]()
2. Binary Relation (only on relations that contain 2 identical domains)
    - [reflexive]()
    - [irreflexive]()
    - [coreflexive]()
    - [symmetric]()
    - [antiSymmetric]()
    - [aSymmetric]()
    - [transitive]()
    - [total]()
    - [connex]()
    - [Euclidean]()
    - [serial]()
    - [equivalence]()
    - [partialOrder]()
