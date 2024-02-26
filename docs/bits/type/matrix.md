# matrix 

 Takes a list of (integer, boolean or enumerated) domains for its indices and a domain for the entries.
 
 Defined by 
 ``` matrix indexed by [COMMA SEPARATED DOMAIN LIST] of [DOMAIN]```
 as demonstrated [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/letting_domain.ipynb).

 or by explicity defining values in square brackets like 
 ``` letting M be [0,1]```
The domain used to index the elements may be specifed too, such that.
```[0,1]``` is the same as ```[0,1; int(1..2)]``` but different from ```[0,1; int(0..1)]```

They are not ordered, but can be compared using equality operators. 
Two matrices are only equal if their indices are the same.

Read more about matrix domains [here](https://conjure.readthedocs.io/en/latest/essence.html#types) and see them demonstrated [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/matrix.ipynb).