# ** (power)

This is the exponent operator.

```
a ** b 
```

will return ```a```<sup>```b```</sup> if ```a``` and ```b``` are positive integers.

Savile Row will flag negative exponents (negative values of ```b```) as errors. However, conjure will not flag this as an error.

``` x ** y = x * (x ** (y-1))``` holds if ```x``` is an integer and ```y``` is a positive integer.
This ensures that ```x ** 0 = 1``` for all ```x```.

For more information see [here](https://conjure.readthedocs.io/en/latest/essence.html#arithmetic-operators).

See this in action [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/powers.ipynb).