# |x| (absolute value)

This is the absolute value operator. It returns the absolute value of ```x```.
This is calculated using
```( 2 * toInt( x >= 0) - 1) * x = |x|```

If ```x``` is a positive integer, then [toInt](https://github.com/conjure-cp/conjure/tree/main/docs/bits/function/toInt.md) returns 1, making ```1 * x = |x|```.

If ```x``` is a non-positive integer, then [toInt](https://github.com/conjure-cp/conjure/tree/main/docs/bits/function/toInt.md) returns 0, making ```-1 * x = |x|```.

This holds for integers ```x``` such that ```|x| <= 2```<sup>```62```</sup>```-2```.
Outside of this range, errors may be flagged up by Savile Row or Minion.

See how this works [here](...).