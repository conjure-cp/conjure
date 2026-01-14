# function

Takes 2 [domains](https://conjure.readthedocs.io/en/latest/essence.html#domains) as arguments: the ```defined``` set and the ```range``` set of the function.
The ```defined``` set is the domain of the function - the set of inputs for the function
the ```range``` set is the codomain of the function - the set of possible outputs for the function

```essence
**function** < comma separated attributes(optional)> <defined> --> <range>
```

There are 3 groups of function attributes: 

- related to cardinality = [size](https://github.com/conjure-cp/conjure/blob/main/docs/bits/L_size.md), [minSize](https://github.com/conjure-cp/conjure/blob/main/docs/bits/L_minSize.md), and [maxSize](https://github.com/conjure-cp/conjure/blob/main/docs/bits/L_maxSize.md). They take arguments.

-  related to function properties = [injective](https://github.com/conjure-cp/conjure/blob/main/docs/bits/L_injective.md), [surjective](https://github.com/conjure-cp/conjure/blob/main/docs/bits/L_surjective.md), [bijective](https://github.com/conjure-cp/conjure/blob/main/docs/bits/L_size.md).

- related to partiality = [total](https://github.com/conjure-cp/conjure/blob/main/docs/bits/L_total.md).

  Functions are partial by default

  You can also explicity define a function as seen [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/functionDemonstration.ipynb).

  See demonstrations of the function attributes [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/functionDemonstration.ipynb).

  Or see functions used to solve the Magic Hexagon Problem [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/magicHexagon.ipynb).

  Much of the information about functions lifted from [the readthedocs](https://conjure.readthedocs.io/en/latest/essence.html).

  
