Simple Permutations
-------------------

Authors: Ruth Hoffmann and Gökberk Koçak

Problem
~~~~~~~

Let a permutation be a sequence of length :math:`n` consisting of numbers between 1 and :math:`n`, in any order with each number occurring exactly once.

An interval in a permutation :math:`{\sigma}` is a factor of contiguous values of σ such that their indices are consecutive.
For example, in the permutation :math:`{\pi} = 346978215`, :math:`{\pi}(4){\pi}(5){\pi}(6) = 978` is an interval, whereas :math:`{\pi}(1){\pi}(2){\pi}(3){\pi}(4) = 3469` is not.
It is easy to see that every permutation of length :math:`n` has intervals of length 0, 1 and :math:`n` , at least. The permutations of length :math:`n` that **only** contain intervals of length 0, 1 and :math:`n` are said to be simple.
So for example the permutation :math:`{\pi} = 346978215` is not simple as we have seen in the example above that it contains an interval, on the other hand :math:`{\sigma} = 526184937` is simple as there are no intervals of length strictly greater than 1, except the whole of :math:`{\sigma}`. 
See :cite:`hoffmann2015thesis` for more information on permutation patterns and simple permutations.


Checking Model
~~~~~~~~~~~~~~

.. code-block:: essence 

    language Essence 1.3
    given n : int
    given perm : sequence (size n) of int
    find result : bool 
    such that
        result = and([ max(subs) - min(subs) + 1 != |subs| | 
            i : int(1..n-1), j : int(2..n), 
            i < j, 
            !(i = 1 /\ j = n),
            letting subs be [perm(k) | k : int(i..j)]]
        )

.. code-block:: essence

    given n : int
    given s : sequence (size n) of int

We define the size of the permutation to be ``n`` and the permutation ``perm`` to be contain integers.

.. code-block:: essence

    find result : bool 

What the model will tell us is that the permutation is simple (``true``) or not.

The idea of our approach is the property of an interval, where when sorted it creates a complete range. 
This can be translated to checking that the difference between the maximal and the minimal elements of the interval is not equal to the cardinality of the interval.

We have one constraint to say that there is only intervals of length 0,1 and ``n``.
This constraint is defined as a matrix comprehension, which will build a matrix consisting of only boolean entries.
We then check the matrix with an ``and`` constraint, to spot if there are any ``false`` entries, which would mean that we have an interval.

.. code-block:: essence

    [ num | num : int(1..5), num != 3 ]

This is an example which creates a 1-dimensional matrix of ``num`` s where none of the entries are ``3``.
We allow also for ``letting`` statements inside the matrix comprehensions, which allow us to define intermediary statements.

.. code-block:: essence

    result = and([ max(subs) - min(subs) + 1 != |subs| | 
        i : int(1..n-1), j : int(2..n), 
        i < j, 
        !(i = 1 /\ j = n),
        letting subs be [perm(k) | k : int(i..j)]]
    )

We extract ``i`` and ``j`` to be the beginning and the end of the interval, and we need to make sure that ``i`` is less than ``j`` to have the right order.
As we do not want to include the whole permutation as an interval, we restrict that ``i`` and ``j`` cannot be simultaneously at the respective ends of the permutation.
The final line of the comprehension sets up the continuous subsequences. 
On the left hand side of the matrix comprehension we use the interval property that when it is turned into a sorted set it is a complete range.


Instances
~~~~~~~~~

.. code-block:: essence

    letting n be 5
    letting s be sequence( 1, 4, 2, 5, 3)

This a non-simple permutation.

.. code-block:: essence

    letting n be 5
    letting s be sequence(2, 4, 1, 5, 3)

This is a simple permutation.

Solving
~~~~~~~

Using the ESSENCE pipeline, we can solve our sample instance by typing the following:

.. code-block:: bash

    conjure solve simple_perm-model.essence simple_perm-instance.essence-param

The result will be saved into a ``.solution`` file which will look something like this:

.. code-block:: essence
