# function

Takes 2 [domains](https://conjure.readthedocs.io/en/latest/essence.html#domains) as arguments: the ```defined``` set and the ```range``` set of the function.
The ```defined``` set is the domain of the function - the set of inputs for the function
the ```range``` set is the codomain of the function - the set of possible outputs for the function

```essence
**function** < comma separated attributes(optional)> <defined> --> <range>
```

There are 3 groups of function attributes: 

- related to cardinality = [size](https://github/conjure-cp/conjure/docs/bits/attribute/L_size.md), [minSize](https://github/conjure-cp/conjure/docs/bits/attribute/L_minSize.md), and [maxSize](https://github/conjure-cp/conjure/docs/bits/attribute/L_maxSize.md). They take arguments.

-  related to function properties = [injective](https://github/conjure-cp/conjure/docs/bits/attribute/L_injective.md), [surjective](https://github/conjure-cp/conjure/docs/bits/attribute/L_surjective.md), [bijective](https://github/conjure-cp/conjure/docs/bits/attribute/L_size.md).

- related to partiality = [total](https://github/conjure-cp/conjure/docs/bits/attribute/L_total.md).

  Functions are partial by default

  You can also explicity define a function as seen [here](https://github/conjure-cp/conjure/docs/notebooks/functionDemonstration.ipynb).

  See demonstrations of the function attributes [here](https://github/conjure-cp/conjure/docs/notebooks/functionDemonstration.ipynb).

  Or see functions used to solve the Magic Hexagon Problem [here](https://github.com/conjure-cp/conjure/tree/main/docs/notebooks/magicHexagon.ipynb).

  Much of the information about functions lifted from [the readthedocs](https://conjure.readthedocs.io/en/latest/essence.html).

  
