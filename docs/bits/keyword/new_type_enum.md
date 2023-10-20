# Enumerated Domains

Enumerated types can be declared in two ways: using a given-enum syntax or using a letting-enum syntax.

The given-enum syntax defers the specification of actual values of the enumerated type until instantiation. With this syntax, an enumerated type can be declared by only giving its name in the problem specification file. In a parameter file, values for the actual members of this type can be given. This allows Conjure to produce a model independent of the values of the enumerated type and only substitute the actual values during parameter instantiation.

The letting-enum syntax can be used to declare an enumerated type directly in a problem specification as well.

Values of an enumerated type cannot contain spaces.
```essence
letting direction be new type enum {North, East, South, West}
find x,y : direction
such that x != y
```
In the example fragment above direction is declared as an enumerated type with 4 members. Two decision variables are declared using direction as their domain and a constraint is posted on the values they can take. Enumerated types support equality, ordering, and successor/predecessor operators; they do not support arithmetic operators.

When an enumerated type is declared, the elements of the type are listed in increasing order.