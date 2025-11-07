# unnamed

Unnamed types are normally used in symmetry breaking at specification level.
Members of this type can't be referred to individually, and only support equality operators.

It is declared using 
```
letting <name> be new type of size <expression>
```
as demonstrated [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/letting_domain.ipynb).

Read more about them [here](https://conjure.readthedocs.io/en/latest/essence.html#unnamed-domains).