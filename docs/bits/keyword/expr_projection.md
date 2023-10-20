# Expression Projection `<-`

The expression projection syntax is used in comprehensions to create a generator from a container data type.
The left side can either be a single variable which
will have the type of a member of the expression or an abstract pattern used to destructure the member type.

Examples:
```essence
[x|x <- [(1,2),(1,3),[1,4]]] $ x : tuple(int,int)
[x+y|(x,y) <- [(1,2),(1,3),[1,4]]] $ x : int , y: int

```

N.B. `:` is used when creating a generator from a domain.