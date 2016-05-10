
.. _essence:

Conjure's input language: Essence
=================================


Problem Specification
---------------------

.. code-block:: bnf

    ProblemSpecification := list(Statement)


Statement
---------

.. code-block:: bnf

    Statement := DeclarationStatement
               | BranchingStatement
               | WhereStatement
               | SuchThatStatement
               | ObjectiveStatement


.. code-block:: bnf

    DeclarationStatement := FindStatement
                          | GivenStatement
                          | LettingStatement
                          | GivenEnum
                          | LettingEnum
                          | LettingUnnamed


.. code-block:: bnf

    FindStatement := "find" Name ":" Domain


.. code-block:: bnf

    FindStatement := "given" Name ":" Domain


.. code-block:: bnf

    LettingStatement := "letting" Name "be" Expression
                      | "letting" Name "be" "domain" Domain


.. code-block:: bnf

    GivenEnum := "given" Name "new type enum"


.. code-block:: bnf

    LettingEnum := "letting" Name "be" "new type enum" list(Name, ",", "{}")


.. code-block:: bnf

    LettingUnnamed := "letting" Name "be" "new type of size" Expression


.. code-block:: bnf

    BranchingStatement := "branching" "on" list(BranchingOn, ",", "[]")


.. code-block:: bnf

    BranchingOn := Name
                 | Expression


.. code-block:: bnf

    WhereStatement := "where" list(Expression, ",")


.. code-block:: bnf

    SuchThatStatement := "such that" list(Expression, ",")


.. code-block:: bnf

    ObjectiveStatement := "minimising" Expression
                        | "maximising" Expression


.. todo::

    Name, Domain, Expression left out from the grammar on purpose.
    Describe them here instead.














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
-------------------

Declarations
^^^^^^^^^^^^

Declaration statements can be used to declare a new decision variable, a new parameter, or an alias.
Parameters are introduced using the `given` keyword followed by a name for the parameter, and a domain for it.
Decision variables are introduced using the `find` keyword, similarly followed by a name and a domain.
It is important to note that the domains of decision variables have to be finite
whereas the domains of parameters can be infinite.

Constraints
^^^^^^^^^^^

Problem constraints are written as comma separated lists of Essence expressions preceded by the keywords `such that`.
Each constraint is an Essence expression of type `bool`.

There is a separate kind of statement for introducing requirements on the values of parameters.
These statements are similar to problem constraints;
they are also written as comma separated lists of Essence expressions,
but they are preceded by the `where` keyword.
Where statements cannot contain references to decision variables.
They can be used to limit valid instances of the problem class.

Objective
^^^^^^^^^

A problem specification can optionally include a single objective declaration.
An objective is declared using either the `minimising` or `maximising` keywords followed by any Essence expression of
type integer.



Essence Domains (and Types)
^^^^^^^^^^^^^^^^^^^^^^^^^^^

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

