# letting domain 

This is how you define a domain (an abstract collection of objects with the same type). Domain attributes further specialise the domain.
For more information about domains and using them in effectively in Essence programs see [here](
https://modref.github.io/papers/ModRef2021_ReformulatingEssenceRobustness.pdf).

This is used like :
```essence 
letting x be domain int(1..10) 
```
Types of domains:

- [boolean](https://github.com/conjure-cp/conjure/blob/main/docs/bits/type/L_bool.md)
- [integer](https://github.com/conjure-cp/conjure/blob/main/docs/bits/type/L_int.md)
- [enumerated](https://github.com/conjure-cp/conjure/blob/main/docs/bits/keyword/new_type_enum.md)
- [unnamed](https://github.com/conjure-cp/conjure/blob/main/docs/bits/type/unnamed.md)
- [tuple](...)
- [record](https://github.com/conjure-cp/conjure/blob/main/docs/bits/type/L_record.md)
- [variant](...)
- [matrix](https://github.com/conjure-cp/conjure/blob/main/docs/bits/type/matrix.md)
- [set](...)
- [multi-set](...)
- [function](https://github.com/conjure-cp/conjure/blob/main/docs/bits/type/function.md)
- [sequence](https://github.com/conjure-cp/conjure/blob/main/docs/bits/type/L_sequence.md)
- [relation](https://github.com/conjure-cp/conjure/blob/main/docs/bits/type/L_relation.md)
- [partition](...)


For more information about different domain types see [here](https://conjure.readthedocs.io/en/latest/essence.html).
To see how to define these types as domains see [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/letting_domain.ipynb).
