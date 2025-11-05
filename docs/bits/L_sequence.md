# sequence

An ordered list of values of a domain.

Can be defined using 
``` sequence (attributes) of <domain>```
The sequence must be bounded, so need a [size](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_size.md) or [maxSize](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_maxSize.md) attribute.

Can also be explicitly defined by a list of values in round brackets as:
```letting x be sequence (1,2,5)```

You can refer to individual values of a sequence using it's index, which starts at one. The first element of a sequence, ```s``` is referred to be ```s(1)```.

Sequences with attributes are treated like a [function](https://github.com/conjure-cp/conjure/blob/main/docs/bits/type/function.md), mapping the index to the corresponding value.
Sequences can have
- cardinal attributes
    - [size](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_size.md)
    - [minSize](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_minSize.md)
    - [maxSize](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_maxSize.md)

- function attributes
    - [injective](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_injective.md)
    - [surjective](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_surjective.md)
    - [bijective](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_bijective.md)

They are [total](https://github.com/conjure-cp/conjure/blob/main/docs/bits/attribute/L_total.md) by default, so don't have a separate ```total``` attribute.

See this demonstrated [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/SequenceDomains.ipynb).
Read more about sequence domains [here](https://conjure.readthedocs.io/en/latest/essence.html#sequence-domains).