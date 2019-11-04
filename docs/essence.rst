
.. _essence:

.. role:: bnff(code)
    :language: bnf


Conjure's input language: Essence
=================================

Conjure works on problem specifications written in Essence.

This section gives a description of Essence.
A more thorough description can be found in the reference paper on Essence
:cite:`frisch2008essence`.

We adopt a BNF-style format to describe all the constructs of the language.
In the BNF format,
we use the "#" character to denote comments,
we use double-quotes for terminal strings,
and we use a ``list`` construct to indicate a list of syntax elements.

The ``list`` construct has three arguments:

#. First argument is the syntax of the items of the list.
#. Second argument is the item separator.
#. Third argument indicates the surrounding bracket for the list. It can be one of round brackets (``()``), curly brackets (``{}``), or square brackets (``[]``).

Only the first argument is mandatory, the rest of the arguments are optional when an item separator or surrounding brackets are not required.

.. code-block:: bnf

    ProblemSpecification := list(Statement)

A problem specification in Essence is composed of a list of statements.
Statements can declare decision variables, parameters or aliases.
They can also post constraints, conditions on parameter values and an objective statement.

The order of statements is largely insignificant, except in one case: names need to be declared before use.
For example a decision variable cannot be used before its declaration.

There are five kinds of statements in Essence.

.. code-block:: bnf

    Statement := DeclarationStatement
               | BranchingStatement
               | SuchThatStatement
               | WhereStatement
               | ObjectiveStatement

Every symbol must be declared before it is used, but otherwise statements can be listed in any order.
A problem specification can contain at most one branching statement.
A problem specification can contain at most one objective statement.


Declarations
------------

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
an alias for an expression or a domain (:bnff:`LettingStatement`),
and enumerated or unnamed types.


Declaring decision variables
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

    FindStatement := "find" Name ":" Domain

A decision variable is declared by using the keyword ``find``, followed by an identifier designating the name of the decision variable, followed by a colon symbol and the domain of the decision variable.
The domains of decision variables have to be finite.

This detail is omitted in the BNF above for simplicity, but a comma separated list of names may also be used to declare multiple decision variables with the same domain in a single find statement. This applies to all declaration statements.


Declaring parameters
~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

    GivenStatement := "given" Name ":" Domain

A parameter is declared in a similar way to decision variables. The only difference is the use of the keyword ``given`` instead of the keyword ``find``.
Unlike decision variables, the domains of parameters do not have to be finite.


Declaring aliases
~~~~~~~~~~~~~~~~~

.. code-block:: bnf

    LettingStatement := "letting" Name "be" Expression
                      | "letting" Name "be" "domain" Domain

An alias for an expression can be declared by using the keyword ``letting``, followed by the name of the alias, followed by the keyword ``be``, followed by an expression. Similarly, an alias for a domain can be declared by including the keyword ``domain`` before writing the domain.

.. code-block:: essence

    letting x be y + z
    letting d be domain set of int(a..b)

In the example above ``x`` is declared as an expression alias for ``y + z`` and ``d`` is declared as a domain alias for ``set of int(a..b)``.


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
Enumerated types support equality, ordering, and successor/predecessor operators; they do not support arithmetic operators.

When an enumerated type is declared, the elements of the type are listed in increasing order.


Declaring unnamed types
~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

    LettingUnnamed := "letting" Name "be" "new type of size" Expression

Unnamed types are a feature of Essence which allow succinct specification of certain types of symmetry.
An unnamed type is declared by giving it a name and a size (i.e. the number of elements in the type).
The members of an unnamed type cannot be referred to individually.
Typically constraints are posted using quantified variables over the whole domain.
Unnamed types only support equality operators; they do not support ordering or arithmetic operators.


Branching statements
--------------------

.. code-block:: bnf

    BranchingStatement := "branching" "on" list(BranchingOn, ",", "[]")

    BranchingOn := Name
                 | Expression

High level problem specification languages typically do not include lower level details such as directives specifying search order.
Essence is such a language, and the reference paper on Essence (:cite:`frisch2008essence`) does not include these search directives at all.

For pragmatic reasons Conjure supports search directives in the form of a branching-on statement, which takes a list of either variable names or expressions.
Decision variables in a branching-on statement are searched using a static value ordering.
Expressions can be used to introduce *cuts*; in which case when solving the model produced by Conjure, the solver is instructed to search for solutions satisfying the cut constraints first, and proceed to searching the rest of the search space later.

A problem specification can contain at most one branching statement.


Constraints
-----------

.. code-block:: bnf

    SuchThatStatement := "such that" list(Expression, ",")

Constraints are declared using the keyword sequence ``such that``, followed by a comma separated list of Boolean expressions.
The syntax for expressions is explained in section `Expressions`_.


Instantiation conditions
------------------------

.. code-block:: bnf

    WhereStatement := "where" list(Expression, ",")

Where statements are syntactically similar to `constraints`_, however they cannot refer to decision variables.
They can be used to post conditions on the parameters of the problem specification.
These conditions are checked during parameter instantiation.


Objective statements
--------------------

.. code-block:: bnf

    ObjectiveStatement := "minimising" Expression
                        | "maximising" Expression

An objective can be declared by using either the keyword ``minimising`` or the keyword ``maximising`` followed by an integer expression.
A problem specification can have at most one objective statement.
If it has none it defines a satisfaction problem, if it has one it defines an optimisation problem.

A problem specification can contain at most one objective statement.


Names
-----

The lexical rules for valid names in Essence are similar to those of most common languages.
A name consists of a sequence of non-whitespace alphanumeric characters (letters or digits) or underscores (``_``).
The first character of a valid name has to be a letter or an underscore.
Names are case-sensitive: Essence treats uppercase and lowercase versions of letters as distinct.


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

The Boolean domain is denoted with the keyword ``bool`` and has two values: ``false`` and ``true``.
The Boolean domain is ordered with ``false`` preceding ``true``.
It is not currently possible to specify an objective with respect to a Boolean value.
If ``a`` is a Boolean variable to minimise or maximise in the objective, use ``toInt(a)`` instead (see `Type conversion operators`_).


Integer domains
~~~~~~~~~~~~~~~

An integer domain is denoted by the keyword ``int``, followed by a list of integer ranges inside round brackets.
The list of ranges is optional, if omitted the integer domain denotes the infinite domain of all integers.

An integer range is either a single integer, or a list of sequential integers with a given lower and upper bound.
The bounds can be omitted to create an open range, but note that using open ranges inside an integer domain declaration creates an infinite domain.

Integer domains can also be constructed using a single set expression inside the round brackets, instead of a list of ranges.
The integer domain contains all members of the set in this case.
Note that the set expression cannot contain references to decision variables if this syntax is used.

Values in an integer domain should be in the range -2**62+1 to 2**62-1 as values outside this range may trigger errors in Savile Row or Minion, and lead to Conjure unexpectedly but silently deducing unsatisfiability.
Intermediate values in an integer expression must also be inside this range.


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

A tuple domain is denoted by the keyword ``tuple``, followed by a list of domains separated by commas inside round brackets.
The keyword ``tuple`` is optional for tuples of arity greater or equal to 2.

When needed, domains inside a tuple are referred to using their positions.
In an n-arity tuple, the position of the first domain is 1, and the position of the last domain is n.

To explicitly specify a tuple, use a list of values inside round brackets, preceded by the keyword ``tuple``.

.. code-block:: essence

   letting s be tuple()
   letting t be tuple(0,1,1,1)


Record domains
~~~~~~~~~~~~~~

Record is a domain constructor, it takes a list of name-domain pairs as arguments.
Records can be of arbitrary arity.
(A name-domain pair is a name, followed by a colon, followed by a domain.)

A record domain is denoted by the keyword ``record``, followed by a list of name-domain pairs separated by commas inside curly brackets.

Records are very similar to tuples; except they use labels for their components instead of positions.
When needed, domains inside a record are referred to using their labels.

To explicitly specify a record, use a list of values inside round brackets, preceded by the keyword ``tuple``.

.. code-block:: essence

   letting s be record{}
   letting t be record{A : int(0..1), B : int(0..2)}


Variant domains
~~~~~~~~~~~~~~~

Variant is a domain constructor, it takes a list of name-domain pairs as arguments.
Variants can be of arbitrary arity.

A variant domain is denoted by the keyword ``variant``, followed by a list of name-domain pairs separated by commas inside curly brackets.

Variants are similar to records but with a very important distinction.
A member of a record domain contains a value for each component of the record, however
a member of a variant domain contains a value for only one of the components of the variant.

Variant domains are similar to `tagged unions <http://en.wikipedia.org/wiki/Tagged_union>`_ in other programming languages.


Matrix domains
~~~~~~~~~~~~~~

Matrix is a domain constructor, it takes a list of domains for its indices and a domain for the entries of the matrix.
Matrices can be of arbitrary dimensionality (greater than 0).

A matrix domain is denoted by the keywords ``matrix indexed by``,
followed by a list of domains separated by commas inside square brackets,
followed by the keyword ``of``, and another domain.

A matrix can be indexed only by integer, Boolean, or enumerated domains.

Matrix domains are the most basic container-like domains in Essence.
They are used when the decision variable or the problem parameter does not have any further relevant structure.
Using another kind of domain is more appropriate for most problem specifications in Essence.

Matrix domains are not ordered, but matrices can be compared using the equality operators.
Note that two matrices are only equal if their indices are the same.

To explicitly specify a matrix, use a list of values inside square brackets.
Optionally, the domain used to index the elements can be specified also.

.. code-block:: essence

   letting M be [0,1,0,-1]
   letting N be [[0,1],[0,-1]]

The matrix ``[0,1]`` is the same as ``[0,1; int(1..2)]``, but distinct from ``[0,1; int(0..1)]``.


Set domains
~~~~~~~~~~~

Set is a domain constructor, it takes a domain as argument denoting the domain of the members of the set.

A set domain is denoted by the keyword ``set``,
followed by an optional comma separated list of set attributes,
followed by the keyword ``of``, and the domain for members of the set.

Set attributes are all related to cardinality: ``size``, ``minSize``, and ``maxSize``.

To explicitly specify a set, use a list of values inside curly brackets.
Values only appear once in the set; if repeated values are specified then they are ignored.

.. code-block:: essence

   letting S be {1,0,1}


Multi-set domains
~~~~~~~~~~~~~~~~~

Multi-set is a domain constructor, it takes a domain as argument denoting the domain of the members of the multi-set.

A multi-set domain is denoted by the keyword ``mset``,
followed by an optional comma separated list of multi-set attributes,
followed by the keyword ``of``, and the domain for members of the multi-set.

There are two groups of multi-set attributes:

#. Related to cardinality: ``size``, ``minSize``, and ``maxSize``.
#. Related to number of occurrences of values in the multi-set: ``minOccur``, and ``maxOccur``.

Since a multi-set domain is infinite without a ``size``, ``maxSize``, or ``maxOccur`` attribute, one of these attributes is mandatory to define a finite domain.

To explicitly specify a multi-set, use a list of values inside round brackets, preceded by the keyword ``mset``.
Values may appear multiple times in a multi-set.

.. code-block:: essence

   letting S be mset(0,1,1,1)


Function domains
~~~~~~~~~~~~~~~~

Function is a domain constructor, it takes two domains as arguments denoting the *defined* and the *range* sets of the function.
It is important to take note that we are using *defined* to mean the domain of the function, and *range* to mean the codomain.

A function domain is denoted by the keyword ``function``,
followed by an optional comma separated list of function attributes,
followed by the two domains separated by an arrow symbol: ``-->``.

There are three groups of function attributes:

#. Related to cardinality: ``size``, ``minSize``, and ``maxSize``.
#. Related to function properties: ``injective``, ``surjective``, and ``bijective``.
#. Related to partiality: ``total``.

Cardinality attributes take arguments, but the rest of the arguments do not.
Function domains are partial by default, and using the ``total`` attribute makes them total.

To explicitly specify a function, use a list of assignments, each of the form ``input --> value``, inside round brackets and preceded by the keyword ``function``.

.. code-block:: essence

   letting f be function(0-->1,1-->0)


Sequence domains
~~~~~~~~~~~~~~~~

Sequence is a domain constructor, it takes a domain as argument denoting the domain of the members of the sequence.

A sequence is denoted by the keyword ``sequence``,
followed by an optional comma separated list of sequence attributes,
followed by the keyword ``of``, and the domain for members of the sequence.

There are 2 groups of sequence attributes:

#. Related to cardinality: ``size``, ``minSize``, and ``maxSize``.
#. Related to function-like properties: ``injective``, ``surjective``, and ``bijective``.

Cardinality attributes take arguments, but the rest of the arguments do not.
Sequence domains are total by default, hence they do not take a separate ``total`` attribute.

Sequences are indexed by a contiguous list of increasing integers, beginning at 1.

To explicitly specify a sequence, use a list of values inside round brackets, preceded by the keyword ``sequence``.

.. code-block:: essence

   letting s be sequence(1,0,-1,2)
   letting t be sequence() $ empty sequence


Relation domains
~~~~~~~~~~~~~~~~

Relation is a domain constructor, it takes a list of domains as arguments.
Relations can be of arbitrary arity.

A relation domain is denoted by the keyword ``relation``,
followed by an optional comma separated list of relation attributes,
followed by the keyword ``of``, and a list of domains separated by the ``*`` symbol inside round brackets.

There are 2 groups of relation attributes:

#. Related to cardinality: ``size``, ``minSize``, and ``maxSize``.
#. Binary relation attributes: ``reflexive``, ``irreflexive``, ``coreflexive``, ``symmetric``, ``antiSymmetric``, ``aSymmetric``, ``transitive``, ``total``, ``connex``, ``Euclidean``, ``serial``, ``equivalence``, ``partialOrder``.

The binary relation attributes are only applicable to relations of arity 2, and are between two identical domains.

To explicitly specify a relation, use a list of tuples, enclosed by round brackets and preceded by the keyword ``relation``.
All the tuples must be of the same type.

.. code-block:: essence

   letting R be relation((1,1,0),(1,0,1),(0,1,1))


Partition domains
~~~~~~~~~~~~~~~~~

Partition is a domain constructor, it takes a domain as an argument denoting the members in the partition.

A partition is denoted by the keyword ``partition``,
followed by an optional comma separated list of partition attributes,
followed by the keyword ``from``, and the domain for the members in the partition.

There are 3 groups of partition attributes:

#. Related to the number of parts in the partition: ``numParts``, ``minNumParts``, and ``maxNumParts``.
#. Related to the cardinality of each part in the partition: ``partSize``, ``minPartSize``, and ``maxPartSize``.
#. Partition properties: ``regular``.

The first and second groups of attributes are related to number of parts and cardinalities of each part in the partition.
The ``regular`` attribute forces each part to be of the same cardinality without specifying the actual number of parts or cardinalities of each part.


Types
-----

Essence is a statically typed language.
A declaration -- whether it is a decision variable, a problem parameter or a quantified variable -- has an associated domain.
From its domain, a type can be calculated.

A type is obtained from a domain by
removing attributes (from set, multi-set, function, sequence, relation, and partition domains),
and removing bounds (from integer and enumerated domains).

In the expression language of Essence, each operator has a typing rule associated with it.
These typing rules are used to both type check expression fragments and to calculate the types of resulting expressions.

For example, the arithmetic operator ``+`` requires two arguments both of which are integers, and the resulting expression is also an integer.
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

A list is a one-dimensional matrix indexed by an integer, starting at 1.
Matrices of dimension k are implemented by a list of matrices of dimension k-1.

.. code-block:: essence

   letting D1 be domain matrix indexed by [int(1..2),int(1..5)] of int(-1..1)
   letting E be domain matrix indexed by [int(1..5)] of int(-1..1)
   letting D2 be domain matrix indexed by [int(1..2)] of E
   find A : D1 such that A[1] = [-1,1,1,0,1], A[2] = [1,1,1,1,1]
   find B : D2 such that B[1] = A[1], B[2] = [0,0,0,0,0]
   letting C be [[-1,1,1,0,1],[0,0,0,0,0]]
   letting a be A[1][1] = -1                   $ true
   letting b be A[1,1] = -1                    $ true
   letting c be C[1] = [-1,1,1,0,1]            $ true
   letting d be B[1] = C[1]                    $ true
   letting e be [A[1],B[2]] = C                $ true
   letting f be B = C                          $ true
   letting F be domain matrix indexed by [int(1..6)] of bool
   find g : F such that g = [a,b,c,d,e,f] $ [true,true,true,true,true,true]


Tuple indexing
~~~~~~~~~~~~~~

Tuples are indexed by a constant integer, starting at 1.
Attempting to access a tuple element via an index that is negative, zero, or too large for the tuple, results in an error.

.. code-block:: essence

   letting s be tuple(0,1,1,0)
   letting t be tuple(0,0,0,1)
   find a : bool such that a = (s[1] = t[1]) $ true


Arithmetic operators
~~~~~~~~~~~~~~~~~~~~

Essence supports the four usual arithmetic operators

 |  ``+``  ``-``  ``*``  ``/``

and also the modulo operator ``%``, exponentiation ``**``.
These all take two arguments and are expressed in infix notation.

There is also the unary prefix operator ``-`` for negation, the unary postfix operator ``!`` for the factorial function, and the absolute value operator ``|x|``.

The arithmetic operators have the usual precedence: the factorial operator is applied first, then exponentiation, then negation, then the multiplication, division, and modulo operators, and finally addition and subtraction.

Exponentiation associates to the right, other binary operators to the left.


Division
^^^^^^^^

Division returns an integer, and the following relationship holds when ``x`` and ``y`` are integers and ``y`` is not zero:

 |  ``(x % y) + y*(x / y) = x``

whenever ``y`` is not zero.
``x / 0`` and ``x % 0`` are expressions that do not have a defined value.
Division by zero may lead to unsatisfiability but is not flagged by either Conjure or Savile Row as an error.

Factorial
^^^^^^^^^

Both ``factorial(x)`` and ``x!`` denote the product of all positive integers up to ``x``, with ``x! = 1`` whenever ``x <= 0``.
The factorial operator cannot be used directly in expressions involving decision variables, so the following

.. code-block:: essence

   find z : int(-1..13)
   such that (z! > 2**28)

is flagged as an error.
However, the following does work:

.. code-block:: essence

   find z : int(-1..13)
   such that (exists x : int(-1..13) . (x! > 2**28) /\ (z=x))

Powers
^^^^^^

When ``x`` is an integer and ``y`` is a positive integer, then ``x**y`` denotes ``x`` raised to the ``y``-th power.
When ``y`` is a negative integer, ``x**y`` is flagged by Savile Row as an error (this includes ``1**(-1)``).
Conjure does not flag negative powers as errors.
The relationship

 |  ``x ** y = x*(x**(y-1))``

holds for all integers ``x`` and positive integers ``y``.
This means that ``x**0`` is always 1, whatever the value of ``x``.

Negation
^^^^^^^^

The unary operator ``-`` denotes negation; when ``x`` is an integer then ``--x = x`` is always true.

Absolute value
^^^^^^^^^^^^^^

When ``x`` is an integer, ``|x|`` denotes the absolute value of ``x``.
The relationship

 | ``(2*toInt(x >= 0) - 1)*x = |x|``

holds for all integers ``x`` such that ``|x| <= 2**62-2``.
Integers outside this range may be flagged as an error by Savile Row and/or Minion.


Comparisons
~~~~~~~~~~~

The inline binary comparison operators ``=``  ``!=``  ``<``  ``<=``  ``>``  ``<=`` can be used to compare two expressions.

The equality operators ``=`` and ``!=`` can be applied to compare two expressions, both taking values in the same domain.
Equality operators are supported for all types.

The equality operators have the same precedence as other logical operators.
This may lead to unintended unsatisfiability or introducing inadvertent solutions.
This is illustrated in the following example, where there are two possible solutions.

.. code-block:: essence

   find a : bool such that a = false  \/ true  $ true or false
   find b : bool such that b = (false \/ true) $ true

The inline binary comparison operators ``<``  ``<=``  ``>``  ``<=`` can be used to compare expressions taking values in an ordered domain.
The expressions must both be integer, both Boolean or both enumerated types.

.. code-block:: essence

    letting direction be new type enum {North, East, South, West}
    find a : bool such that a = ((North < South) /\ (South < West))  $ true
    find b : bool such that b = (false <= true) $ true

The inline binary comparison operators

 | ``<lex`` ``<=lex`` ``>lex`` ``>=lex``

test whether their arguments have the specified relative lexicographic order.

.. code-block:: essence

    find v : matrix indexed by [int(1..2)] of int(1..2)
    such that v <lex [ v[3-i] | i : int(1..2) ] $ v = [1,2]


Logical operators
~~~~~~~~~~~~~~~~~

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
The ``and``, ``or`` and ``xor`` operators can be applied to sets or lists of Boolean values (see `List combining operators`_ for details).
Note that ``<-`` is not a logical operator, but is used in list comprehension syntax.


Set operators
~~~~~~~~~~~~~

The following set operators return Boolean values indicating whether a specific relationship holds:

+--------------------+---------------------------------------------------------+
| ``in``             | test if element is in set                               |
+--------------------+---------------------------------------------------------+
| ``subset``         | test if first set is strictly contained in second set   |
+--------------------+---------------------------------------------------------+
| ``subsetEq``       | test if first set is contained in second set            |
+--------------------+---------------------------------------------------------+
| ``supset``         | test if first set strictly contains second set          |
+--------------------+---------------------------------------------------------+
| ``supsetEq``       | test if first set contains second set                   |
+--------------------+---------------------------------------------------------+

These binary inline operators operate on sets and return a set:

+--------------------+---------------------------------------------------------+
| ``intersect``      | set of elements in both sets                            |
+--------------------+---------------------------------------------------------+
| ``union``          | set of elements in either of the sets                   |
+--------------------+---------------------------------------------------------+

The following unary operator operates on a set and returns a set:

+--------------------+---------------------------------------------------------+
| ``powerSet``       | set of all subsets of a set (including the empty set)   |
+--------------------+---------------------------------------------------------+

When ``S`` is a set, then ``|S|`` denotes the non-negative integer that is the cardinality of ``S`` (the number of elements in ``S``).
When ``S`` and ``T`` are sets, ``S - T`` denotes their set difference, the set of elements of ``S`` that do not occur in ``T``.

Examples:

.. code-block:: essence

   find a : bool such that a = (1 in {0,1}) $ true
   find b : bool such that b = ({0,1} subset {0,1}) $ false
   find c : bool such that c = ({0,1} subsetEq {0,1}) $ true
   find d : bool such that d = ({0,1} supset {}) $ true
   find e : bool such that e = ({0,1} supsetEq {1,0}) $ true
   find A : set of int(0..6) such that A = {1,2,3} intersect {3,4} $ {3}
   find B : set of int(0..6) such that B = {1,2,3} union {3,4} $ {1,2,3,4}
   find S : set of set of int(0..2) such that S = powerSet({0}) $ {{},{0}}
   find x : int(0..9) such that x = |{0,1,2,1,2,1}| $ 3
   find T : set of int(0..9) such that T = {0,1,2} - {2,3} $ {0,1}


Sequence operators
~~~~~~~~~~~~~~~~~~

For two sequences ``s`` and ``t``, ``s subsequence t`` tests whether the list of values taken by ``s`` occurs in the same order in the list of values taken by ``t``, and ``s substring t`` tests whether the list of values taken by ``s`` occurs in the same order and contiguously in the list of values taken by ``t``.

When ``S`` is a sequence, then ``|S|`` denotes the number of elements in ``S``.

.. code-block:: essence

   letting s be sequence(1,1)
   letting t be sequence(2,1,3,1)
   find a : bool such that s subsequence t $ true
   find b : bool such that s substring t $ false
   find c : int(1..10) such that c = |t| $ 4


Enumerated type operators
~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------------+---------------------------------------------------------+
| ``pred``           | predecessor of this element in an enumerated type       |
+--------------------+---------------------------------------------------------+
| ``succ``           | successor of this element in an enumerated type         |
+--------------------+---------------------------------------------------------+

Enumerated types are ordered, so they support comparisons and the operators `max` and `min`.

.. code-block:: essence

   letting D be new type enum { North, East, South, West }
   find a : D such that a = succ(East) $ South
   find b : bool such that b = (max([North, South]) > East) $ true


Multiset operators
~~~~~~~~~~~~~~~~~~

The following operators take a single argument:

+--------------------+---------------------------------------------------------+
| ``hist``           | histogram of multi-set/matrix                           |
+--------------------+---------------------------------------------------------+
| ``max``            | largest element in ordered set/multi-set/domain/list    |
+--------------------+---------------------------------------------------------+
| ``min``            | smallest element in ordered set/multi-set/domain/list   |
+--------------------+---------------------------------------------------------+

The following operator takes two arguments:

+-------------------------+----------------------------------------------------+
| ``freq``                | counts occurrences of element in multi-set/matrix  |
+-------------------------+----------------------------------------------------+

Examples:

.. code-block:: essence

   letting S be mset(0,1,-1,1)
   find x : int(0..1) such that freq(S,x) = 2 $ 1
   find y : int(-2..2) such that y = max(S) - min(S) $ 2
   find z : int(-2..2) such that z = max([1,2]) $ 2


Type conversion operators
~~~~~~~~~~~~~~~~~~~~~~~~~

+--------------------+---------------------------------------------------------+
| ``toInt``          | maps ``true`` to 1, ``false`` to 0                      |
+--------------------+---------------------------------------------------------+
| ``toMSet``         | set/relation/function to multi-set                      |
+--------------------+---------------------------------------------------------+
| ``toRelation``     | function to relation; ``function(a --> b)`` becomes     |
|                    | ``relation((a,b))``                                     |
+--------------------+---------------------------------------------------------+
| ``toSet``          | multi-set/relation/function to set; ``mset(0,0,1)``     |
|                    | becomes ``{0,1}``                                       |
+--------------------+---------------------------------------------------------+

It is currently not possible to use an operator to directly invert ``toRelation`` or ``toSet`` when applied to a function, or ``toSet`` when applied to a relation.
By referring to the set of tuples of a function ``f`` indirectly by means of ``toSet(f)``, the set of tuples of a relation ``R`` by means of ``toSet(R)``, or the relation corresponding to a function ``g`` by ``toRelation(g)``, it is possible to use the declarative forms

.. code-block:: essence

   find R : relation of (int(0..1) * int(0..1))
   such that toSet(R) = {(0,0),(0,1),(1,1)}

   find f : function int(0..1) --> int(0..1)
   such that toSet(f) = {(0,0),(1,1)}

   find g : function int(0..1) --> int(0..1)
   such that toRelation(g) = relation((0,0),(1,1))

to indirectly recover the relation or function that corresponds to a set of tuples, or the function that corresponds to a relation.
This will fail to yield a solution if a function corresponding to a set of tuples or relation is sought, but that set of tuples or relation does not actually determine a function.
An error results if a relation corresponding to a set of tuples is sought, but not all tuples have the same number of elements.


Function operators
~~~~~~~~~~~~~~~~~~

+-------------------------+----------------------------------------------------+
| ``defined``             | set of values for which function is defined        |
+-------------------------+----------------------------------------------------+
| ``image``               | ``image(f,x)`` is the same as ``f(x)``             |
+-------------------------+----------------------------------------------------+
| ``imageSet``            | ``imageSet(f,x)`` is ``{f(x)}`` if ``f(x)`` is     |
|                         | defined, or empty if ``f(x)`` is not defined       |
+-------------------------+----------------------------------------------------+
| ``inverse``             | test if two functions are inverses of each other   |
+-------------------------+----------------------------------------------------+
| ``preImage``            | set of elements mapped by function to an element   |
+-------------------------+----------------------------------------------------+
| ``range``               | set of values of function                          |
+-------------------------+----------------------------------------------------+
| ``restrict``            | function restricted to a domain                    |
+-------------------------+----------------------------------------------------+

Operators ``defined`` and ``range`` yield the sets of values that a function maps between.
For all functions ``f``, the set ``toSet(f)`` is contained in the Cartesian product of sets ``defined(f)`` and ``range(f)``.

For a function ``f`` and a domain ``D``, the expression ``restrict(f,D)`` denotes the function that is defined on the values in ``D`` for which ``f`` is defined, and that also coincides with ``f`` where it is defined.

.. code-block:: essence

   letting f be function(0-->1,3-->4)
   letting D be domain int(0,2)
   find g : function int(0..4)-->int(0..4) such that
     g = restrict(f, D) $ function(0-->1)
   find a : bool such that $ true
     a = ( (defined(g) = defined(f) intersect toSet([i | i : D]))
       /\ (forAll x in defined(g) . g(x) = f(x)) )

Applying ``image`` to values for which the function is not defined may lead to unintended unsatisfiability.
The Conjure specific ``imageSet`` operator is useful for partial functions to avoid unsatisfiability in these cases.
The original Essence definition allows ``image`` to represent the image of a function with respect to either an element or a set.
Conjure does not currently support taking the ``image`` or ``preImage`` of a function with respect to a set of elements.

The ``inverse`` operator tests whether its function arguments are inverses of each other.

.. code-block:: essence

   find a : bool such that a = inverse(function(0-->1),function(1-->0)) $ true
   find b : bool such that b = inverse(function(0-->1),function(1-->1)) $ false


Matrix operators
~~~~~~~~~~~~~~~~

The following operator returns a matrix:

+--------------------+---------------------------------------------------------+
| ``flatten``        | list of entries from matrix                             |
+--------------------+---------------------------------------------------------+

``flatten`` takes 1 or 2 arguments.
With one argument, ``flatten`` returns a list containing the entries of a matrix with any number of dimensions, listed in the lexicographic order of the tuples of indices specifying each entry.
With two arguments ``flatten(n,M)``, the first argument ``n`` is a constant integer that indicates the depth of flattening: the first ``n+1`` dimensions are flattened into one dimension.
Note that ``flatten(0,M) = M`` always holds.
The one-argument form works like an unbounded-depth flattening.

The following operators yield Boolean values:

+-------------------------+----------------------------------------------------+
| ``allDiff``             | test if all entries of a list are different        |
+-------------------------+----------------------------------------------------+
| ``alldifferent_except`` | test if all entries of a list differ,              |
|                         | possibly except value specified in second argument |
+-------------------------+----------------------------------------------------+

The following illustrate ``allDiff`` and ``alldifferent_except``:

.. code-block:: essence

   find a : bool such that a = allDiff([1,2,4,1]) $ false
   find b : bool such that b = alldifferent_except([1,2,4,1], 1) $ true


Partition operators
~~~~~~~~~~~~~~~~~~~

+-------------------------+----------------------------------------------------+
| ``apart``               | test if a list of elements are not all contained   |
|                         | in one part of the partition                       |
+-------------------------+----------------------------------------------------+
| ``participants``        | union of all parts of a partition                  |
+-------------------------+----------------------------------------------------+
| ``party``               | part of partition that contains specified element  |
+-------------------------+----------------------------------------------------+
| ``parts``               | partition to its set of parts                      |
+-------------------------+----------------------------------------------------+
| ``together``            | test if a list of elements are all in the same     |
|                         | part of the partition                              |
+-------------------------+----------------------------------------------------+

Examples:

.. code-block:: essence

   letting P be partition({1,2},{3},{4,5,6})
   find a : bool such that a = apart({3,5},P) /\ !together({1,2,5},P) $ true
   find b : set of int(1..6) such that b = participants(P) $ {1,2,3,4,5,6}
   find c : set of int(1..6) such that c = party(4,P) $ {4,5,6}
   find d : bool such that d = ({{1,2},{3},{4,5,6}} = parts(P)) $ true
   find e : bool such that e = (together({1,7},P) \/ apart({1,7},P)) $ false

These semantics follow the original Essence definition.
In contrast, in older versions of Conjure the relationship

 | ``apart(L,P) = !together(L,P)``

held for all lists ``L`` and partitions ``P``.


List combining operators
~~~~~~~~~~~~~~~~~~~~~~~~

Each of the operators

 | ``sum    product    and    or    xor``

applies an associative combining operator to elements of a list or set.
A list may also be given as a comprehension that specifies the elements of a set or domain that satisfy some conditions.

The following relationships hold for all integers ``x`` and ``y``:

 | ``sum([x,y]) = (x + y)``
 | ``product([x,y]) = (x * y)``

The following relationships hold for all Booleans ``a`` and ``b``:

 | ``and([a,b]) = (a /\ b)``
 | ``or([a,b]) = (a \/ b)``
 | ``xor([a,b]) = ((a \/ b) /\ !(a /\ b))``

Examples:

.. code-block:: essence

   find x : int(0..9) such that x = sum( {1,2,3} ) $ 6
   find y : int(0..9) such that y = product( [1,2,4] ) $ 8
   find a : bool such that a = and([xor([true,false]),or([false,true])]) $ true

Quantification over a finite set or finite domain of values is supported by ``forAll`` and ``exists``.
These quantifiers yield Boolean values and are internally treated as ``and`` and ``or``, respectively, applied to the lists of values corresponding to the set or domain.
The following snippets illustrate the use of quantifiers.

.. code-block:: essence

   find a : bool such that a = forAll i in {0,1,2} . i=i*i $ false
   find b : bool such that b = exists i : int(0..4) . i*i=i $ true

The same variable can be reused for multiple quantifications, as a quantified variable has scope that is local to its quantifier.
Older versions of Savile Row do not support using the same name both for quantification and as a global decision variable in a ``find``.

An alternative quantifier-like syntax

 | ``sum i in I . f(i)``

where ``I`` is a set, or for domains

 | ``sum i : int(0..3) . f(i)``

is supported for the ``sum`` and ``product`` operators.


Comprehensions
~~~~~~~~~~~~~~

A list can be constructed by means of a comprehension.
A list comprehension is declared by using square brackets ``[`` and ``]`` as for other lists, inside which is a generator expression possibly involving some parameter variables, followed by ``|``, followed by a comma (``,``) separated sequence of conditions.
Each condition is a Boolean expression.
The value of a list comprehension is a list containing all the values of the generator expression corresponding to those values of the parameter variables for which all the conditions evaluate to ``true``.
Comprehension conditions may include ``letting`` statements, which have local scope within the comprehension.

In a Boolean expression controlling a comprehension, if ``L`` is a list then ``v <- L`` behaves similarly to how the expression ``v in toMSet(L)`` is treated in a quantification.
If ``c`` is a list comprehension, then ``|c|`` denotes the number of values in ``c``.

Examples of list comprehensions:

.. code-block:: essence

   find x : int(0..999) such that x = product( [i-1 | i <- [5,6,7]] ) $ 120
   letting M be [1,0,0,1,0]
   letting I be domain int(1..5)
   find y : int(0..9) such that y = sum( [toInt((i=j) /\ (M[j]>0)) | i : I, j <- M] ) $ 2
   find a : bool such that a = and([u<v | (u,v) <- [(0,1),(2**10,2**11),(-1,1)] ]) $ true
   find m : int(0..999) such that m = | [M[i] | i : I, M[i] != 0] | $ 2
   find n : int(0..999) such that n = | toSet([M[i] | i : I, M[i] != 0]) | $ 1
   find b : bool such that b =
       or([ (x=y) | i : I, letting x be i, letting y be M[i] ]) $ true


