
.. _essence:

.. role:: bnff(code)
    :language: bnf


Conjure's input language: Essence
=================================

Conjure works on problem specifications written in Essence.

This section gives a description of Essence, a more thorough description can be found in the reference paper on Essence
is :cite:`frisch2008essence`.

We adopt a BNF-style format to describe all the constructs of the language.
In the BNF format,
we use the "#" character to denote comments,
we use double-quotes for terminal strings,
and we use a ``list`` construct to indicate a list of syntax elements.

The ``list`` construct has two variants:

#. First variant takes two arguments where the first argument is the syntax of the items of the list and second argument is the item separator.
#. Second variant takes an additional third argument which indicates the surrounding bracket for the list. The third argument can be one of round brackets (``()``), curly brackets (``{}``), or square brackets (``[]``).

Problem Specification
---------------------

.. code-block:: bnf

    ProblemSpecification := list(Statement)

A problem specification in Essence is composed of a list of statements.
Statements can declare decision variables, parameters or aliases.
They can also post constraints, conditions on parameter values and an objective statement.

The order of statements is largely insignificant, except in one case: names need to be declared before use.
For example a decision variable cannot be used before its declaration.

A problem specification can contain at most one objective statement.


Statements
----------

.. code-block:: bnf

    Statement := DeclarationStatement
               | BranchingStatement
               | WhereStatement
               | SuchThatStatement
               | ObjectiveStatement

There are five kinds of statements in Essence.


Declarations
~~~~~~~~~~~~

.. code-block:: bnf

    DeclarationStatement := FindStatement
                          | GivenStatement
                          | LettingStatement
                          | GivenEnum
                          | LettingEnum
                          | LettingUnnamed

A declaration statement can be used to declare
a decision variable (:bnff:`FindStatement`),
a parameter (:bnff:`GivenStatement`),
an alias to an expression or a domain (:bnff:`LettingStatement`),
and enumerated or unnamed types.
The syntax for each of these declaration statements are given in the following.


Declaring decision variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

    FindStatement := "find" Name ":" Domain

A decision variable is declared by using the "find" keyword, followed by an identifier designating the name of the decision variable, followed by a colon symbol and the domain of the decision variable.
The domains of decision variables have to be finite.

This detail is omitted in the BNF above for simplicity, but a comma separated list of names may also be used to declare multiple decision variables with the same domain in a single find statement. This applies to all declaration statements.


Declaring parameters
~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

    GivenStatement := "given" Name ":" Domain

A parameter is declared in a similar way to decision variables. The only difference is the use of the "given" keyword instead of the "find" keyword.
Unlike decision variables, the domains of parameters do not have to be finite.


Declaring aliases
~~~~~~~~~~~~~~~~~

.. code-block:: bnf

    LettingStatement := "letting" Name "be" Expression
                      | "letting" Name "be" "domain" Domain

An alias to an expression can be declared by using the "letting" keyword, followed by the name of the alias, followed by the keyword "be", followed by an expression. Similarly, an alias to a domain can be declared by including the keyword "domain" before writing the domain.

.. code-block:: essence

    letting x be y + z
    letting d be domain set of int(a..b)

In the example above ``x`` is declared as an expression alias to ``y + z`` and ``d`` is declared as a domain alias to ``set of int(a..b)``.


Declaring enumerated types
~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

    GivenEnum := "given" Name "new type enum"

    LettingEnum := "letting" Name "be" "new type enum" list(Name, ",", "{}")

Enumerated types can be declared in two ways: using a given-enum syntax or using a letting-enum syntax.

The given-enum syntax defers the specification of actual values of the enumerated type until instantiation.
With this syntax, an enumerated type can be declared by only giving its name in the problem specification file.
In a parameter file, values for the actual members of this type can be given.
This allows Conjure to produce a model independent of the values of the enumerated type and only substitute the actual values during parameter instantiation.

The letting-enum syntax can be used to declare an enumerated type directly in a problem specification as well.

.. code-block:: essence

    letting direction be new type enum {North, East, South, West}
    find x,y : direction
    such that x != y

In the example fragment above ``direction`` is declared as an enumerated type with 4 members.
Two decision variables are declared using ``direction`` as their domain and a constraint is posted on the values they can take.
Enumerated types support equality, ordering and successor/predecessor operators; they do not support arithmetic operators.


Declaring unnamed types
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

    LettingUnnamed := "letting" Name "be" "new type of size" Expression

Unnamed types are a feature of Essence which allow succinct specification of certain types of symmetry.
An unnamed type is declared by giving it a name and a size (i.e. the number of elements in the type).
The members of an unnamed type cannot be referred to individually.
Typically constraints are posted using quantified variables over the whole domain.
Unnamed types only support equality operators; they do not support ordering or arithmetic operators.


Search directives
~~~~~~~~~~~~~~~~~

.. code-block:: bnf

    BranchingStatement := "branching" "on" list(BranchingOn, ",", "[]")

    BranchingOn := Name
                 | Expression

Essence is a high level problem specification language and typically it doesn't include lower level details such as search directives.
In fact the reference paper on Essence (:cite:`frisch2008essence`) does not include these search directives at all.

For pragmatic reasons we support adding search directives in the form of a branching-on statement, which takes a list of either variable names or expressions.
Decision variables in a branching-on statement are searched using a static value ordering.
Expressions can be used to introduce *cuts*; in which case when solving the model produced by Conjure, the solver is instructed to search for solutions satisfying the cut constraints first, and proceed to searching the rest of the search space later.

A problem specification can contain at most one branching-on statement.


Instantiation conditions
~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

    WhereStatement := "where" list(Expression, ",")

Where statements are syntactically similar to constraints, however they cannot refer to decision variables.
They can be used to post conditions on the parameters of the problem specification.
These conditions are checked during parameter instantiation.


Constraints
~~~~~~~~~~~

.. code-block:: bnf

    SuchThatStatement := "such that" list(Expression, ",")

Constraints are declared using the keyword sequence "such that", followed by a comma separated list of Boolean expressions.
The syntax for expressions is explained in the later sections.


.. code-block:: bnf

    ObjectiveStatement := "minimising" Expression
                        | "maximising" Expression

An objective can be declared by using either the "minimising" or the "maximising" keyword followed by an integer expression.
A problem specification can have at most one objective statement.
If it has none it defines a satisfaction problem, if it has one it defines an optimisation problem.


Names
-----

The lexical rules for valid names in Essence are very similar to those of most common languages.
A name consists of a sequence of non-whitespace alphanumeric characters (letters or digits).
The first character of a valid name has to be a letter or an underscore character ('_').


Domains
-------

.. code-block:: bnf

    Domain := "bool"
            | "int" list(Range, ",", "()")
            | "int" "(" Expression ")"
            | Name list(Range, ",", "()") # the Name refers to an enumerated type
            | Name                        # the Name refers to an unnamed type
            | "tuple" list(Domain, ",", "()")
            | "record" list(NameDomain, ",", "{}")
            | "variant" list(NameDomain, ",", "{}")
            | "matrix indexed by" list(Domain, ",", "[]") "of" Domain
            | "set" list(Attribute, ",", "()") "of" Domain
            | "mset" list(Attribute, ",", "()") "of" Domain
            | "function" list(Attribute, ",", "()") Domain "-->" Domain
            | "sequence" list(Attribute, ",", "()") "of" Domain
            | "relation" list(Attribute, ",", "()") "of" list(Domain, "*", "()")
            | "partition" list(Attribute, ",", "()") "from" Domain

    Range := Expression
           | Expression ".."
           | ".." Expression
           | Expression ".." Expression

    Attribute := Name
               | Name Expression

    NameDomain := Name ":" Domain


Essence contains a rich selection of domain constructors, which can be used in an arbitrarily nested fashion to create domains for problem parameters, decision variables, quantified expressions and comprehensions.
Quantified expressions and comprehensions are explained under `Expressions`_.

Domains can be finite or infinite, but infinite domains can only be used when declaring of problem parameters.
The domains for both decision variables and quantified variables have to be finite.

Some kinds of domains can take an optional list of attributes.
An attribute is either a label or a label with an associated value.
Different kinds of domains take different attributes.

Multiple attributes can be used in a single domain.
Using contradicting values for the attribute values may result in an empty domain.

In the following, each kind of domain is described in a subsection of its own.

Boolean domains
~~~~~~~~~~~~~~~

A Boolean domain is denoted with the keyword "bool", and has two values: "false" and "true".

Integer domains
~~~~~~~~~~~~~~~

An integer domain is denoted by the keyword "int", followed by a list of integer ranges inside round brackets.
The list of ranges is optional, if omitted the integer domain denotes the infinite domain of all integers.

An integer range is either a single integer, or a list of sequential integers with a given lower and upper bound.
The bounds can be omitted to create an open range, but note that using open ranges inside an integer domain declaration creates an infinite domain.

Integer domains can also be constructed using a single set expression inside the round brackets, instead of a list of ranges.
The integer domain contains all members of the set in this case.
Note that the set expression cannot contain references to decision variables if this syntax is used.

Enumerated domains
~~~~~~~~~~~~~~~~~~

Enumerated types are declared using the syntax given in `Declaring enumerated types`_.

An enumerated domain is denoted by using the name of the enumerated type, followed by a list of ranges inside round brackets.
The list of ranges is optional, if omitted the enumerated domain denotes the finite domain containing all values of the enumerated type.

A range is either a single value (member of the enumerated type), or a list of sequential values with a given lower and upper bound.
The bounds can be omitted to create an open range, when an open range is used the omitted bound is considered to be the same as the corresponding bound of the enumerated type.

Unnamed domains
~~~~~~~~~~~~~~~

Unnamed types are declared using the syntax given in `Declaring unnamed types`_.

An unnamed domain is denoted by using the name of the unnamed type.
It does not take a list of ranges to limit the values in the domain, an unnamed domain always contains all values in the corresponding unnamed type.

Tuple domains
~~~~~~~~~~~~~

Tuple is a domain constructor, it takes a list of domains as arguments.
Tuples can be of arbitrary arity.

A tuple domain is denoted by the keyword "tuple", followed by a list of domains separated by commas inside round brackets.
The keyword "tuple" is optional for tuples of arity greater or equal to 2.

When needed, domains inside a tuple are referred to using their positions.
In an n-arity tuple, the position of the first domain is 1, and the position of the last domain is n.

Record domains
~~~~~~~~~~~~~~

Record is a domain constructor, it takes a list of name-domain pairs as arguments.
Records can be of arbitrary arity.

A record domain is denoted by the keyword "record", followed by a list of name-domain pairs separated by commas inside curly brackets.

Records are very similar to tuples; except they use labels for their components instead of positions.
When needed, domains inside a record are referred to using their labels.

Variant domains
~~~~~~~~~~~~~~~

Variant is a domain constructor, it takes a list of name-domain pairs as arguments.
Variants can be of arbitrary arity.

A variant domain is denoted by the keyword "variant", followed by a list of name-domain pairs separated by commas inside curly brackets.

Variants are similar to records but with a very important distinction.
A member of a record domain contains a value for each component of the record, however
a member of a variant domain contains a value for only one of the components of the variant.

Variant domains are similar to `tagged unions <http://en.wikipedia.org/wiki/Tagged_union>`_ in other programming languages.

Matrix domains
~~~~~~~~~~~~~~

Matrix is a domain constructor, it takes a list of domains for its indices and a domain for the entries of the matrix.
Matrices can be of arbitrary dimensionality (greater than 0).

A matrix domain is denoted by the keywords "matrix indexed by",
followed by a list of domains separated by commas inside square brackets,
followed by the keyword "of", and another domain.

Matrix domains are the most basic container-like domains in Essence.
They are used when the decision variable or the problem parameter does not have any further relevant structure.
Using another kind of domain is more appropriate for most problem specifications in Essence.

Set domains
~~~~~~~~~~~

Set is a domain constructor, it takes a domain as argument denoting the domain of the members of the set.

A set domain is denoted by the keyword "set",
followed by an optional comma separated list of set attributes,
followed by the keyword "of", and the domain for members of the set.

Set attributes are all related to cardinality: "size", "minSize", and "maxSize".

Multi-set domains
~~~~~~~~~~~~~~~~~

Multi-set is a domain constructor, it takes a domain as argument denoting the domain of the members of the multi-set.

A multi-set domain is denoted by the keyword "mset",
followed by an optional comma separated list of multi-set attributes,
followed by the keyword "of", and the domain for members of the multi-set.

There are two groups of multi-set attributes:

#. Related to cardinality: "size", "minSize", and "maxSize".
#. Related to number of occurrences of values in the multi-set: "minOccur", and "maxOccur".

Since a multi-set domain is infinite without a "size", "maxSize", or "maxOccur" attribute, one of these attributes is mandatory to define a finite domain.

Function domains
~~~~~~~~~~~~~~~~

Function is a domain constructor, it takes two domains as arguments denoting the *defined* and the *range* sets of the function.
It is important to take note that we are using *defined* to mean the domain of the function, and *range* to mean the codomain.

A function domain is denoted by the keyword "function",
followed by an optional comma separated list of function attributes,
followed by the two domains separated by an arrow symbol: "-->".

There are three groups of function attributes:

#. Related to cardinality: "size", "minSize", and "maxSize".
#. Related to function properties: "injective", "surjective", and "bijective".
#. Related to partiality: "total".

Cardinality attributes take arguments, but the rest of the arguments do not.
Function domains are partial by default, and using the "total" attribute makes them total.

To explicitly specify a sequence, use a parenthesized set of assignments, each of the form `input --> value``.

Sequence domains
~~~~~~~~~~~~~~~~

Sequence is a domain constructor, it takes a domain as argument denoting the domain of the members of the sequence.

A sequence is denoted by the keyword "sequence",
followed by an optional comma separated list of sequence attributes,
followed by the keyword "of", and the domain for members of the sequence.

There are 2 groups of sequence attributes:

#. Related to cardinality: "size", "minSize", and "maxSize".
#. Related to function-like properties: "injective", "surjective", and "bijective".

Cardinality attributes take arguments, but the rest of the arguments do not.
Sequence domains are total by default, hence they do not take a separate "total" attribute.

Sequences are indexed by a contiguous list of increasing integers, beginning at 1.
To explicitly specify a sequence, use a parenthesized list of tuples, each tuple being a pair (index,value).

Relation domains
~~~~~~~~~~~~~~~~

Relation is a domain constructor, it takes a list of domains as arguments.
Relations can be of arbitrary arity.

A relation domain is denoted by the keyword "relation",
followed by an optional comma separated list of relation attributes,
followed by the keyword "of", and a list of domains separated by the "*" symbol inside round brackets.

There are 2 groups of relation attributes:

#. Related to cardinality: "size", "minSize", and "maxSize".
#. Binary relation attributes: "reflexive", "irreflexive", "coreflexive"
                             , "symmetric" , "antiSymmetric" , "aSymmetric"
                             , "transitive", "total", "connex", "Euclidean", "serial", "equivalence", "partialOrder".

The binary relation attributes are only applicable to relations of arity 2, and are between two identical domains.
To explicitly specify a relation, use a parenthesized list of tuples.

Partition domains
~~~~~~~~~~~~~~~~~

Partition is a domain constructor, it takes a domain as an argument denoting the members in the partition.

A partition is denoted by the keyword "partition",
followed by an optional comma separated list of partition attributes,
followed by the keyword "from", and the domain for the members in the partition.

There are N groups of partition attributes:

#. Related to the number of parts in the partition: "numParts", "minNumParts", and "maxNumParts".
#. Related to the cardinality of each part in the partition: "partSize", "minPartSize", and "maxPartSize".
#. Partition properties: "regular".

The first and second groups of attributes are related to number of parts and cardinalities of each part in the partition.
The "regular" attribute forces each part to be of the same cardinality without specifying the actual number of parts or cardinalities of each part.

Types
-----

Essence is a statically typed language.
A declaration -- whether it is a decision variable, a problem parameter or a quantified variable -- has an associated domain.
From its domain, a type can be calculated.

A type is obtained from a domain by
removing attributes (from set, multi-set, function, sequence, relation, and partition domains),
and removing bounds (from integer and enumerated domains).

In the expression language of Essence, each operator has a typing rules associated with it.
These typing rules are used to both type check expression fragments and to calculate the types of resulting expressions.

For example, the arithmetic operator "+" requires two arguments both of which are integers, and the resulting expression is also an integer.
So if ``a``, and ``b`` are integers ``a + b`` is also an integer.
Conjure gives a type error otherwise.

Using these typing rules every Essence expression can be checked for type correctness statically.

Expressions
-----------

.. code-block:: bnf

    Expression := Literal
                | Name
                | Quantification
                | Comprehension Expression [GeneratorOrCondition]
                | Operator

    Operator := ...


(In preparation)



Matrix indexing
~~~~~~~~~~~~~~~

A 1D matrix is indexed by an integer.
Matrices of dimension k are implemented by 1D matrices of dimension k-1.


Tuple indexing
~~~~~~~~~~~~~~


Arithmetic operators
^^^^^^^^^^^^^^^^^^^^

As well as the four usual binary inline arithmetic operations

 |  ``+``  ``-``  ``*``  ``/``

Essence also supports the modulo operator ``%`` and exponentiation ``**``.
There is also the unary prefix operation ``-``, the unary postfix operation ``!``, and the absolute value operator.

Division returns an integer, and the following relationship holds when ``x`` and ``y`` are integers and ``y`` is not zero:

 |  ``(x % y) + y*(x / y) = x``

whenever ``y`` is not zero.
``x / 0`` and ``x % 0`` are expressions that do not have a defined value.
Note that division by zero is not flagged as an error.

Both ``fact(x)`` and ``x!`` denote the product of all positive integers up to ``x``, with ``x! = 1`` whenever ``x <= 0``.
When ``x`` is an integer and ``y`` is a positive integer, then ``x**y`` denotes ``x`` raised to the ``y``-th power.
``x**0`` is always 1.
When ``y`` is a negative integer, ``x**y`` is not defined and is flagged by Savile Row as an error (this includes ``1**(-1)``).
The relationship

 |  ``x ** y = x*(x**(y-1))``

holds for all integers ``x`` and positive integers ``y``.

The unary operator ``-`` denotes negation; when ``n`` is an integer then ``--n = n`` is always true.

When ``n`` is an integer, ``|n|`` denotes the absolute value of ``n``.
The relationship

 | ``(2*toInt(x >= 0) - 1)*x = |x|``

holds for all integers ``x`` such that ``|x| <= 2**30-2``.


Comparisons
^^^^^^^^^^^

The inline binary comparison operators

 | ``=``  ``!=``  ``<``  ``<=``  ``>``  ``<=``

can be applied to integer and enumerated types.
Sets/multisets/relations/functions/domains?

The inline binary comparison operators

 | ``<lex`` ``<=lex`` ``>lex`` ``>=lex``

test whether their arguments have the specified relative lexicographic order.


Logical operators
^^^^^^^^^^^^^^^^^

+--------------------+------------------------------------+
| ``/\``             | and                                |
+--------------------+------------------------------------+
| ``\/``             | or                                 |
+--------------------+------------------------------------+
| ``->``             | implication                        |
+--------------------+------------------------------------+
| ``<->``            | if and only if                     |
+--------------------+------------------------------------+
| ``!``              | negation                           |
+--------------------+------------------------------------+

Logical operators operate on Boolean valued expressions, returning a Boolean value ``false`` or ``true``.
Negation is unary prefix, the others are binary inline.


Set operations
^^^^^^^^^^^^^^

These set operations return Boolean values indicating whether a specific relationship holds.

+--------------------+---------------------------------------------------------+
| ``in``             | test if element is in set                               |
+--------------------+---------------------------------------------------------+
| ``subset``         | test if set is strictly contained in second set         |
+--------------------+---------------------------------------------------------+
| ``subsetEq``       | test if set is contained in second set                  |
+--------------------+---------------------------------------------------------+
| ``supset``         | test if first set strictly contains second set          |
+--------------------+---------------------------------------------------------+
| ``supsetEq``       | test if first set contains second set                   |
+--------------------+---------------------------------------------------------+

These binary inline operations operate on sets or domains and return a set:

+--------------------+---------------------------------------------------------+
| ``intersect``      | set of elements in both sets/domains                    |
+--------------------+---------------------------------------------------------+
| ``union``          | set of elements in either of the sets                   |
+--------------------+---------------------------------------------------------+

When ``S`` is a set, then ``|S|`` denotes the non-negative integer that is the cardinality of ``S`` (the number of elements in ``S``).
When ``S`` and ``T`` are sets, ``S - T`` denotes their set difference, the set of elements of ``S`` that do not occur in ``T``.


Sequence operators
^^^^^^^^^^^^^^^^^^

For two sequences ``s`` and ``t``, ``subsequence(s,t)`` tests whether there is a function ``f`` such that the list of values taken by ``s`` occurs in the same order in the list of values taken by ``t``, and ``substring(s,t)`` tests whether the list of values taken by ``s`` occurs in the same order and contiguously in the list of values taken by ``t``.


Operators taking one argument
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------------+---------------------------------------------------------+
| ``allDiff``        | test if all entries of 1D matrix are different          |
+--------------------+---------------------------------------------------------+
| ``factorial``      | ``factorial(n)`` is the same as ``n!``                  |
+--------------------+---------------------------------------------------------+
| ``hist``           | histogram of ?                                          |
+--------------------+---------------------------------------------------------+
| ``max``            | largest element in set/multiset/domain, if ordered      |
+--------------------+---------------------------------------------------------+
| ``min``            | smallest element in set/multiset/domain, if ordered     |
+--------------------+---------------------------------------------------------+
| ``parts``          | partition to its set of parts                           |
+--------------------+---------------------------------------------------------+
| ``participants``   | union of all parts of a partition                       |
+--------------------+---------------------------------------------------------+
| ``powerSet``       | set of all subsets of a set (including the empty set)   |
+--------------------+---------------------------------------------------------+
| ``defined``        | set of values for which function is defined             |
+--------------------+---------------------------------------------------------+
| ``range``          | set of values of function                               |
+--------------------+---------------------------------------------------------+


Enumerated type operators
~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------------+---------------------------------------------------------+
| ``pred``           | predecessor of this element in an enumerated type       |
+--------------------+---------------------------------------------------------+
| ``succ``           | successor of this element in an enumerated type         |
+--------------------+---------------------------------------------------------+

Enumerated types are ordered, so they support comparisons and the operators `max` and `min`.


Type conversion
~~~~~~~~~~~~~~~

+--------------------+---------------------------------------------------------+
| ``toInt``          | maps ``true`` to 1, ``false`` to 0                      |
+--------------------+---------------------------------------------------------+
| ``toMSet``         | set/relation/function to multiset                       |
+--------------------+---------------------------------------------------------+
| ``toRelation``     | function to relation; `{a --> b}` becomes `((a,b))`     |
+--------------------+---------------------------------------------------------+
| ``toSet``          | multiset/relation/function to set                       |
+--------------------+---------------------------------------------------------+

It is currently not possible to directly invert ``toMSet``, ``toRelation``, and ``toSet``.
For instance, although it is possible to describe the set of tuples of a function ``f`` by means of ``toSet(f)``, there is currently no inverse operation to turn the tuples back into a function.
However, it is possible to use the declarative forms

.. code-block:: essence

   find R : relation int(0..1) --> int(0..1)
   such that toSet(R) = {(0,0),(0,1),(1,1)}

   find f : function int(0..1) --> int(0..1)
   such that toSet(f) = {(0,0),(1,1)}

to refer to the relation, multiset, or function that corresponds to a set of tuples.
This will fail to yield a solution if a function corresponding to a set of tuples is sought, but that set of tuples does not actually determine a function.


Operators with two arguments
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

+-------------------------+----------------------------------------------------+
| ``active``              | ?                                                  |
+-------------------------+----------------------------------------------------+
| ``catchUndef``          | ?                                                  |
+-------------------------+----------------------------------------------------+
| ``alldifferent_except`` | test if all entries of 1D matrix differ,           |
|                         | possibly except value specified in second argument |
+-------------------------+----------------------------------------------------+
| ``apart``               | test if two elements are in different parts of     |
|                         | the partition                                      |
+-------------------------+----------------------------------------------------+
| ``inverse``             | test if two functions are inverses of each other   |
+-------------------------+----------------------------------------------------+
| ``freq``                | counts occurrences of element in multiset          |
+-------------------------+----------------------------------------------------+
| ``image``               | ``image(f,x)`` is the same as ``f(x)``             |
+-------------------------+----------------------------------------------------+
| ``imageSet``            | ``imageSet(f,x)`` is ``{f(x)}`` if ``f(x)`` is     |
|                         | defined, or empty if ``f(x)`` is not defined:      |
|                         | especially useful for partial functions            |
+-------------------------+----------------------------------------------------+
| ``party``               | part of partition that contains specified element  |
+-------------------------+----------------------------------------------------+
| ``preImage``            | set of elements mapped by function to an element   |
+-------------------------+----------------------------------------------------+
| ``restrict``            | function restricted to a set of values             |
+-------------------------+----------------------------------------------------+
| ``together``            | test if two elements are in the same part of the   |
|                         | partition                                          |
+-------------------------+----------------------------------------------------+

The original Essence definition allows ``image`` to represent the image of a function with respect to either an element or a set.
Conjure does not currently support taking the image with respect to a set of elements.

+--------------------+---------------------------------------------------------+
| ``concatenate``    | ?                                                       |
+--------------------+---------------------------------------------------------+
| ``flatten``        | 1D matrix of entries from matrix                        |
+--------------------+---------------------------------------------------------+

``flatten`` takes 1 or 2 arguments.  For the 2-argument form, the first argument must be a constant integer.
With one argument, ``flatten`` returns a 1D matrix containing the entries of a matrix with any number of dimensions, listed in the lexicographic order of the tuples of indices specifying each entry.
With two arguments ``flatten(n,M)``, the first argument n indicates the depth of flattening: the first n+1 dimensions are flattened into one dimension.
The one-argument form works like an unbounded-depth flattening.
Note that ``flatten(0,M) = M`` always holds?


List combining operators
~~~~~~~~~~~~~~~~~~~~~~~~

Each of these operators applies a combining operator to elements of a list.

The following operators are all behave of this form, taking a list as argument and computing the operation over all the elements.  Each element appears once but the ordering of elements is not specified: no reliance should be placed on a particular ordering.

 | ``sum``  ``product``  ``and``  ``or``  ``xor``

The following equivalences hold:

 | ``sum(x,y) = x + y``
 | ``product(x,y) = x * y``
 | ``and(a,b) = a /\ b``
 | ``or(a,b) = a \/ b``
 | ``xor(a,b) = (a \/ b) /\ !(a /\ b)``

The preferred syntax is comprehension style, providing a list as the argument to the operator.
The list may be specified by means of a comprehension, as the elements of a set or domain that satisfy a given condition.
The following are examples of valid syntax:

 | ``sum( [P(i) | i <- I] )``
 | ``sum( [P(i) \/ Q(j) | i in I, j : D] )``

An alternative quantifier-like syntax

 | ``sum i in I . P(i)``

is supported for ``sum`` and ``product``.

