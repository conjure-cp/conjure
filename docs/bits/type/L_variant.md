# variant

Similar to a [record](https://github.com/conjure-cp/conjure/blob/main/docs/bits/type/L_record.md) but can only be 1 value, of 1 domain at a time. It is also like a [tagged union](https://en.wikipedia.org/wiki/Tagged_union).

It is defined by 

```
letting <name> be domain variant {<list of domain : name pairs>}
```

as shown [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/letting_domain.ipynb)

Like [records](https://github.com/conjure-cp/conjure/blob/main/docs/bits/type/L_record.md), you can refer to a domain using 
```[lebel]```
as demonstrated [here](https://github.com/conjure-cp/conjure/blob/main/docs/notebooks/VariantDomains.ipynb).

Read more about variants [here](https://conjure.readthedocs.io/en/latest/essence.html#variant-domains).