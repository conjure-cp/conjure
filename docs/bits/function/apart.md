# apart

test if a list of elements are not all contained in one part of the partition

```
letting P be partition({1,2},{3},{4,5,6})
find a: bool such that a = apart({3,5},P) $true
```
The result of these syntaxes is true, because elements 3 and 5 ({3,5}) are not all contained in one part of the partition ({1,2},{3},{4,5,6})

See it demonstrated [here](https://github.com/conjure-cp/conjure/blob/main/docs/Partition_operators.ipynb).
