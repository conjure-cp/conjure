
.. _essence:

.. role:: bnff(code)
    :language: bnf


Conjure's input language: Essence
=================================

Conjure works on problem specifications written in Essence.

This section gives a description of Essence, a more thorough description can be found in the reference paper on Essence
is :cite:`frisch2008essence`.

We  adopt a BNF-style format to describe all the constructs of the language.
In the BNF format,
we use the "#" character to denote comments,
we use double-quotes for terminal strings,
and we use a ``list`` construct to indicate a list of syntax elements.

The ``list`` construct has two variants:

1. First variant takes two arguments where the first argument is the syntax of the items of the list and second argument is the item separator.
2. Second variant takes an additional third argument which indicates the surrounding bracket for the list. The third argument can be one of round brackets (``()``), curly brackets (``{}``), or square brackets (``[]``).

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
In a parameter file the actual values that will be the members of this type can be given.
This will allow Conjure to produce a model independent of the values of the enumerated type and only substitute the actual values during parameter instantiation.

The letting-enum syntax can be used to declare an enumerated type directly in a problem specification as well.

.. code-block:: essence

    letting direction be new type enum {North, East, South, West}
    find x,y : direction
    such that x != y

In the example fragment above ``direction`` is declared as an enumerated type with 4 members.
Two decision variables are declared using ``direction`` as their domain and a constraint is posted on the values they can take.
Enumerated types only support equality and ordering operators; they do not support arithmetic operators.


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
Decision variables in a branching-on statement will be searched using a static value ordering.
Expressions can be used to introduce *cuts*; the model produced by Conjure will search for solutions satisfying the cut constraints first, and only after that will proceed to searching the rest of the search space.

A problem specification can contain at most one branching-on statement.


Instantiation conditions
~~~~~~~~~~~~~~~~~~~~~~~~

.. code-block:: bnf

    WhereStatement := "where" list(Expression, ",")

Where statements are syntactically similar to constraints, however they cannot refer to decision variables.
They can be used to post conditions on the parameters of the problem specification.
These conditions will be checked during parameter instantiation.


Constraints
~~~~~~~~~~~

.. code-block:: bnf

    SuchThatStatement := "such that" list(Expression, ",")

Constraints are declared using the keyword sequence "such that", followed by a comma separated list of boolean expressions.
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

Essence contains a rich selection of domain constructors, which can be used in an arbitrarily nested fashion to create domains for problem parameters, decision variables, quantified expressions and comprehensions.
Quantified expressions and comprehensions are explained under `Expressions`_.

Domains can be finite or infinite, but infinite domains can only be used when declaring of problem parameters.
The domains for both decision variables and quantified variables have to be finite.

Boolean domains
~~~~~~~~~~~~~~~

A Boolean domain is denoted with the keyword "bool", and has two values: "false" and "true".

Integer domains
~~~~~~~~~~~~~~~

An integer domain is denoted by the keyword "int", followed by a list of integer ranges inside round brackets.
The list of ranges is optional, if omitted the integer domain denotes the infinite domain of all integers.

An integer range is either a single integer, or a list of sequential integers with a given lower and upper bound.
The bounds can be omitted to create an open range, but note that using open ranges inside an integer domain declaration will create an infinite domain.

Integer domains can also be constructed using a single set expression inside the round brackets, instead of a list of ranges.
The integer domain will contain all members of the set in this case.
Note that the set expression cannot contain references to decision variables if this syntax is used.

Enumerated domains
~~~~~~~~~~~~~~~~~~

Enumerated types are declared using the syntax given in `Declaring enumerated types`_.

An enumerated domain is denoted by using the name of the enumerated type, followed by a list of ranges inside round brackets.
The list of ranges is optional, if omitted the enumerated domain denotes the finite domain containing all values of the enumerated type.

A range is either a single value (member of the enumerated type), or a list of sequential values with a given lower and upper bound.
The bounds can be omitted to create an open range, when an open range is used the omitted bound will be the same as the corresponding bound of the enumerated type.

Unnamed domains
~~~~~~~~~~~~~~~

Unnamed types are declared using the syntax given in `Declaring unname types`_.

An unnamed domain is denoted by using the name of the unnamed type.
It does not take a list of ranges to limit the values in the domain, an unnamed domain will always contain all values in the corresponding unnamed type.





(In preparation)


Espressions
-----------

(In preparation)











OLD STUFF FROM HERE ON




Essence is a high level problem specification language with a rich collection of domains,
and a rich collection of operators operating on these domains.

A problem specification in Essence defines a class of problems.
A separate file is used to provide values for parameters defined in a problem specification to produce a problem instance.

Conjure (forward ref?) works on problem specifications of classes when performing automated modelling.
Once a concrete CP model is produced for a given Essence specification, it can be instantiated by multiple parameters
to solve different instances of the same problem class.

A problem specification in Essence is a composed of a sequence of statements.

Kinds of Statements

Declarations

Declaration statements can be used to declare a new decision variable, a new parameter, or an alias.
Parameters are introduced using the `given` keyword followed by a name for the parameter, and a domain for it.
Decision variables are introduced using the `find` keyword, similarly followed by a name and a domain.
It is important to note that the domains of decision variables have to be finite
whereas the domains of parameters can be infinite.

Constraints

Problem constraints are written as comma separated lists of Essence expressions preceded by the keywords `such that`.
Each constraint is an Essence expression of type `bool`.

There is a separate kind of statement for introducing requirements on the values of parameters.
These statements are similar to problem constraints;
they are also written as comma separated lists of Essence expressions,
but they are preceded by the `where` keyword.
Where statements cannot contain references to decision variables.
They can be used to limit valid instances of the problem class.

Objective

A problem specification can optionally include a single objective declaration.
An objective is declared using either the `minimising` or `maximising` keywords followed by any Essence expression of
type integer.



Essence Domains (and Types)

Essence is a statically typed language.
The type of every expression can be determined without looking into the context the expression appears in.

Here is a list of all domain constructors in the language.

- Boolean
- Integer
- Tuple
- Matrix
- Set
- Multi-Set
- Function
- Relation
- Partition

In addition to these domains, new Enumerated and Unnamed domains can also be declared.

