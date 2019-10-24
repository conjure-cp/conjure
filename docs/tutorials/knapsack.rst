


The Knapsack problem
-------------------------

The Knapsack problem is a classical combinatorial optimisation problem, often used in areas of resource allocation. A basic variant of the Knapsack problem is defined as follows:

- Given:
    #. A set of items, each with a weight and a value,
    #. A maximum weight which we call *capacity*,
- find a set of the items such that
    #. The sum of the weights of the items in our set is less than or equal to the capacity,and 
    #. Maximising the sum of the values of the items.

Informally, think about putting items in a sack such that we maximise the total value of the sack whilst not going over the sack's weight limit.

We begin by showing the entire problem as defined in Essence:


.. code-block:: essence

    given items new type enum
    given weight : function (total) items --> int
    given gain : function (total) items --> int
    given capacity : int
    find picked : set of items
    maximising sum i in picked . gain(i)
    such that (sum i in picked . weight(i)) <= capacity

Going through the problem line by line:

We begin by defining the parameters to the problem.  Parameters are given in a separate file, allowing different instances of the same problem to be solved without having to change the specification.

Each parameter is denoted with the *given* keyword.

.. code-block:: essence

    given items new type enum

This line says that a list of items will be provided in the parameter file as an enum type.  Enums are good for labeling items where it makes no sense to attribute a value to each item.  So instead of using integers to represent each item, we use an enum. 

.. code-block:: essence

    given weight : function (total) items --> int

Another parameter, a function that maps from each item to an integer, we will treat these integers as weights.  Since we are describing integers that will be given in the parameter file, no domain (lower/upper bound) is required.


.. code-block:: essence

    given capacity : int

Another parameter -- a weight limit.


.. code-block:: essence

    find picked : set of items

The *find* keyword denotes decision variables, these are the variables for which  the solver will search for a valid assignment. As is common in Essence problems, our entire problem is modelled using one decision variable.  *picked* is the name of the variable, its type is *set of items*; a set of any size who's elements are taken from the *items* domain.  Note, the maximum cardinality of the set is implicitly the size of the *items* domain.

.. code-block:: essence

    maximising sum i in picked . gain(i)

The *maximising* keyword denotes the objective for the solver; a value for the solver to *maximise*.  *minimise* is also a valid objective keyword.  The expression ``sum i in picked .`` is a quantifier. The ``sum`` says that the values we produce should be summed together.  The ``i in picked`` says we want to list out every element of the set ``picked``.  The expression given to the ``sum`` are described by the expression that follows the full-stop (``.``).  In this case, we are asking for the image of ``i`` in the ``gain`` function.  That is, for each item in the set, we are looking up the integer value that the item maps to in the ``gain`` function and summing these integers.
 

.. code-block:: essence

    such that (sum i in picked . weight(i)) <= capacity

The ``such that`` keyword denotes a constraint.  Here the constraint is formulated in a similar manner to the objective.  We are quantifying over the set of chosen items ``picked``, looking up the value that the item maps to in the ``weights`` function and summing these values to together.  We enforce that the result of the sum must be less than or equal to the capacity ``<= capacity``.

Note that you can post multiple constraints either by using commas between each constraint ``,`` or by reusing the keyword ``such that``.