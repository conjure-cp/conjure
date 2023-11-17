# int

Integer domains can be defined by
```
int (<ranges list>)
```

If no range is provided, then represents infinite domain of integers.

An integer range is either a single integer 
```
int(n)
```
 
or a list of sequential integers with a given lower and upper bound (inclusive).
 ```
 int(a..b, c..d, ...)
 ```

Values must be between -2<sup>62</sup> + 1 and 2<sup>62</sup> - 1.

See this demonstrated [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/IntegerDomains.ipynb).
