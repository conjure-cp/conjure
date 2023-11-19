# tuple

Defined by a list of comma separated domains in brackets as 
```
tuple( 1, 2, 3)
```

The ```tuple``` keyword is not needed if it contains two or more domains.
This can be done implicitly as shown [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/letting_domain.ipynb), or explicitly as shown [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/TupleDomains.ipynb).

A domain inside a tuple can be referred to using its position index, with the first domain in position 1, and the nth domain in position n.
```
letting y be tuple(1,2,3)
y[1] = 1
```
This is demonstrated [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/TupleDomains.ipynb).