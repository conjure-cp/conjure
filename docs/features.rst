
.. _features:

Features
========

This section lists some features of Conjure.

Some of these are due to features of Conjure's input language Essence, and the need to support those. If you are not familiar with Essence, please see :ref:`essence`.

Problem classes
---------------

Often, when we think of problems we think of a *class* of problems rather than a single problem.
For example, Sudoku is a class of puzzles.
There are many different Sudoku *instances*, with different clues.
However, all Sudoku instances share the same set of rules.
Describing the puzzle of Sudoku to somebody who doesn't know the rules of the game generally does not depend on a given set of clues.

Similarly, problem specifications in Essence are written for a class of problems instead of a single problem instance.
For Sudoku, the rules (everything on a row/column/sub-grid has to be distinct) are encoded once.
The clues are specified as *parameters* to the problem specification, together with appropriate assignment statements to incorporate the clues into the problem.

Many tools (solvers and/or modelling assistants) support this separation by having a parameterised problem specification in a file and separate data/parameter file specifying an instance of the problem.
Conjure uses ``*.essence`` files for the problem specification, and ``*.param`` files for the parameter file.

Although a lot of tools support this kind of a separation, they generally work by instantiating a problem specification before operating on it.
Conjure is different than most tools in this regard: it operates on parameterised problem specifications directly.
It reads in a parameterised Essence file, and outputs one or more parameterised Essence' files.
To solve an Essence' model provided by Conjure, Essence-level parameter files need to be translated to Essence'-level parameter files by running Conjure once per parameter file.

Savile Row accepts a parameterised model and a separate parameter file, and performs the instantiation.
The output model and the translated parameter file from Conjure can be directly used when running Savile Row.


High level of abstraction
-------------------------

Conjure's input language is Essence.
Essence provides abstract domain types like sets, multi-sets, functions, sequences, relations, partitions, records, and variants.
These abstract domain types also support domain attributes like cardinality for set-like domains and injectivity/surjectivity for functions, to enable concise specification of a problem.
Essence also provides more primitive domain types like Booleans, integers, enumerated types, and matrices, that are supported by most CP solvers and modelling assistants.

In addition to abstract domain types, Essence also provides operators that operate on parameters or decision variables with abstract domains.
For example, set membership, subset, function inverse, and relation projection are provided to enable specification of problem constraints abstractly.

The high level of abstraction offered by Essence allows its users to specify problems without having to make a lot of low level *modelling decisions*.


Arbitrarily nested types
------------------------

The abstract domain types provided by Essence are domain constructors: they take another domain as an argument to construct a new domain.
For example the domain ``set of D`` represents a set of values from the domain ``D``, and a ``relation of (D1 * D2 * D3)`` represents a relation between values of domains ``D1``, ``D2``, and ``D3``.

Using these domain constructors, domains of arbitrary nesting can be created.
Conjure does not have a limit on the level of nesting in the domains.
But keep in mind: a several levels nested domain might look tiny whereas the combinatorial object it represents may be huge.


Automatic symmetry breaking
---------------------------

During its modelling process, a decision variable with an abstract domain type is *represented* using a collection of decision variables with more primitive domain types.
For example the domain ``set (size n) of D``, which represents a set of ``n`` values from the domain ``D``, can be represented using the domain ``matrix indexed by [int(1..n)] of D``.
Performing this modelling transformation requires rewriting the rest of the model.
Moreover, it introduces symmetry into the model, since a set implies a collection of distinct values whereas the matrix does not.
To break this symmetry, Conjure introduces strict ordering constraints on adjacent entries of the matrix.

This is one of the simplest examples of automated symmetry breaking performed by Conjure.
Conjure breaks all the symmetry introduced by modelling transformations like this one.

Another example is the domain ``set of D`` without the explicit ``size`` attribute.
Since the number of elements in this set is not known, Conjure cannot simply use a matrix to represent this domain.
There are multiple ways to represent this domain.
One representation is to use an integer to partition the entries of the matrix into two:
entries before the index pointed by this integer are regarded to be in the set, and
entries after this position are regarded to be irrelevant.

It is important to post constraints on the irrelevant entries to fix them to a certain value.
Not doing this introduces more symmetry.
Conjure breaks this kind of symmetry by introducing constraints to fix their values.


Multiple models
---------------

Conjure is able to generate multiple Essence' models starting from a single Essence problem specification.
Each model generated by Conjure can be used to solve the initial problem specified in Essence.

This feature is important because often a problem can be modelled in several ways, and it is difficult to know what a *good* model is for a given problem.
Constraint programming experts spend considerable amounts of time developing models.
It is common to create multiple models to compare how well they perform for a problem.
A good model is then chosen only after several models have been considered.

Moreover, a single good model may not even exist for certain classes of problems.
The choice of the model may depend on the instances we are interested in solving.

Lastly, instead of trying to pick a single good model a portfolio of models may be chosen with complementary strengths to exploit parallelism.

Conjure is able to produce multiple models mainly

- by having choices between multiple representations of decision variable domains, and
- by having choices between translating constraint expressions in multiple ways.

Both domain representation and constraint translation mechanisms are implemented using a rule based system inside Conjure to ease the addition of new modelling idioms.


Automated channelling
---------------------

While modelling a problem using constraint programming, it is often possible to model a certain decision using multiple encodings.
When different encodings with complementary strengths are available, experts can utilise this flexibility by using one encoding for parts of the formulation and another encoding for the rest of the formulation.
When multiple encodings of a single decision are used in a single model, *channelling* constraints are added to ensure consistency between encodings.

In Conjure, decision variables with abstract domain types can very often be represented in multiple ways.
For each occurrence of a decision variable, Conjure considers all representation options.
If a decision variable is used more than once, this means that the decision variable can be represented in multiple ways in a single Essence' model.

When multiple representations are used, channelling constraints are generated by Conjure automatically.
These constraints make sure that different representations of the same abstract combinatorial object have the same abstract value.


Extensibility
-------------

The modelling transformations of Conjure are implemented using a rule-based system.

There are two main kinds of rules in Conjure:

representations selection rules
    to specify domain transformations,
expression refinement rules
    to rewrite constraint expressions depending on their domain representations.

Moreover, Conjure contains a collection of **horizontal rules**, which are representation independent expression refinement rules.
Thanks to horizontal rules, the number of representation dependent expression refinement rules are kept to a small number.

Conjure's architecture is designed to make adding both representation selection rules and expression refinement rules easy.


Multiple target solvers
-----------------------

The ability to target multiple solvers is not a feature of Conjure by itself, but a benefit it gains thanks to being a part of a state-of-the-art constraint programming tool-chain.
Each Essence' model generated by Conjure can be solved using `Savile Row <http://savilerow.cs.st-andrews.ac.uk>`_ together with one of its target solvers.

Savile Row can directly target Minion, Gecode (via fzn-gecode), and any SAT solver that supports the DIMACS format.
It can also output Minizinc, and this output can be used to target a number of different solvers using the mzn2fzn tool.


