
.. _tut_np:


Number puzzle
-------------

We first show how to solve a classic `word addition <https://en.wikipedia.org/wiki/Verbal_arithmetic>`_ puzzle, due to Dudeney :cite:`dudeney1924puzzle`.
This is a small toy example, but already illustrates some interesting features of Conjure.

::

        SEND
     +  MORE
     -------
     = MONEY

Here each letter represents a numeric digit in an addition, and we are asked to find an assignment of digits to letters so that the number represented by the digits SEND when added to the number represented by MORE yields the number represented by MONEY.

Initial model
~~~~~~~~~~~~~

We are looking for a mapping (a function) from letters to digits.
We can represent the different letters as an enumerated type, with each letter appearing only once.
We then need to express what it means for the digits of the sum to behave as we expect; a natural approach is to introduce carry digits and use those to express the sum digit-wise.
There are only at most two digits and a carry digit being added at each step, so the carry digits cannot be larger than 2.

.. code-block:: essence

   language Essence 1.3
   letting letters be new type enum {S,E,N,D,M,O,R,Y}
   find f : function letters --> int(0..9)
   such that
       f(S) + f(E) + f(N) + f(D) +
       f(M) + f(O) + f(R) + f(E) =
       f(M) + f(O) + f(N) + f(E) + f(Y)

Each Essence specification can optionally contain a declaration of which dialect of Essence it is written in.
The current version of Essence is 1.3.
We leave out this declaration in the remaining examples to avoid repetition.

This model is stored in ``sm1.essence``; let's use Conjure to find the solution:

.. code-block:: bash

    conjure solve -ac sm1.essence

Unless we specify what to call the solution, it is saved as ``sm1.solution``.

.. code-block:: essence

   letting carry1 be 0
   letting carry2 be 0
   letting carry3 be 0
   letting carry4 be 0
   letting f be function(S --> 0, E --> 0, N --> 0, D --> 0, M --> 0,
     O --> 0, R --> 0, Y --> 0)

This is clearly not what we wanted.
We haven't specified all the constraints in the problem!


Identifying a missing constraint
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In these kinds of puzzles, usually we need each letter to map to a different digit: we need an injective function.
Let's replace the line

.. code-block:: essence

   find f : function letters --> int(0..9)

by

.. code-block:: essence

   find f : function (injective) letters --> int(0..9)

and save the result in file ``sm2.essence``.
Now let's run Conjure again on the new model:

.. code-block:: bash

    conjure solve -ac sm2.essence

This time the solution ``sm2.solution`` looks more like what we wanted:

.. code-block:: bash

   letting carry1 be 1
   letting carry2 be 0
   letting carry3 be 1
   letting carry4 be 0
   letting f be function(S --> 2, E --> 8, N --> 1, D --> 7, M --> 0,
     O --> 3, R --> 6, Y --> 5)

Final model
~~~~~~~~~~~

There is still something strange with ``sm2.essence``.
We usually do not allow a number to begin with a zero digit, but the solution maps M to 0.
Let's add the missing constraints to file ``sm3.essence``:

.. code-block:: essence

   letting letters be new type enum {S,E,N,D,M,O,R,Y}
   find f : function (injective) letters --> int(0..9)
   find carry1,carry2,carry3,carry4 : int(0..2)
   such that
              f(D) + f(E) = f(Y) + 10*carry1,
     carry1 + f(N) + f(R) = f(E) + 10*carry2,
     carry2 + f(E) + f(O) = f(N) + 10*carry3,
     carry3 + f(S) + f(M) = f(O) + 10*carry4,
     carry4 = f(M),
     M > 0, S > 0

Let's try again:

.. code-block:: bash

   conjure solve -ac sm3.essence

This now leads to the solution we expected:

.. code-block:: essence

   letting carry1 be 1
   letting carry2 be 1
   letting carry3 be 0
   letting carry4 be 1
   letting f be function(S --> 9, E --> 5, N --> 6, D --> 7, M --> 1,
     O --> 0, R --> 8, Y --> 2)

Note that the solution includes both the mapping we were looking for, as well as values for the carry digits that were introduced to express the constraints.

Finally, let's check that there are no more solutions:

.. code-block:: bash

   conjure solve -ac sm3.essence --number-of-solutions=all

This confirms that there is indeed only one solution.
As an exercise, verify that the first two models have multiple solutions, and that the solution given by the third model is among these.
(The first has 1155 solutions, the second 25.)

