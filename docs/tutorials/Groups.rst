
Semigroups, Monoids and Groups
------------------------------

Authors: Chris Jefferson and Alice Lynch

This tutorial discusses how to model semigroups, monoids and groups in Essence.

The Problem
~~~~~~~~~~~

Semigroups, monoids and groups are all examples of binary operations, with added conditions.

We will begin by building a binary operation. A binary relation ``R`` on a domain ``S`` is a two-argument function which maps two elements of ``S`` to a third element of ``S``. We will make ``S`` be the integers from ``1`` to ``n``, for some given ``n``.

.. code-block:: essence

  given n : int
  letting S be domain int(1..n)

  find R : function(total) (S,S) --> S

We make a new type of size ``n`` to represent the set the operation is defined on. We then define a function from ``(S,S)`` to ``S``. Technically, this function doesn't take two arguments - it takes a single argument which is a pair of values from ``S``. This is mathematically the same, but will change how we use ``R``.

We will begin by creating a solution to this, for ``n = 4``.

Result::

  letting R be
        function((1, 1) --> 1, (1, 2) --> 1, (1, 3) --> 1, (1, 4) --> 1,
                 (2, 1) --> 1, (2, 2) --> 1, (2, 3) --> 1, (2, 4) --> 1,
                 (3, 1) --> 1, (3, 2) --> 1, (3, 3) --> 1, (3, 4) --> 1,
                 (4, 1) --> 1, (4, 2) --> 1, (4, 3) --> 1, (4, 4) --> 1)

At the moment this is quite boring, as the function can take any value at all! Asking Conjure how many solutions this problem has is unreasonable, but we can figure it out with maths: $$4^{16} = 4,294,967,296$$. Let's try adding some constraints.


Semigroups
~~~~~~~~~~

The simplest object we will consider is a **semigroup**. A semigroup adds one constraint to our binary operation, **associativity**. A binary operation is associative if for all i,j and k in S,  R(i,R(j,k)) = R((R(i,j),k). This might look very abstract, but it is true of many binary operations, for example given integers i,j and k, (i+j)+k = i+(j+k), and (i * j) * k = i * (j * k).

We begin by saying we want to check ``forAll i,j,k: S``. The strangest bit is all of the brackets seem doubled. Your vision isn't failing, this is because ``R`` is a one argument function (and we use ``R(x)`` to apply ``R`` to ``x``), but ``R`` takes a tuple as its argument (which we write as ``(i,j)``), so to apply ``R`` to ``i`` and ``j`` we write ``R((i,j))``.

.. code-block:: essence

  given n : int
  letting S be domain int(1..n)

  find R : function(total) (S,S) --> S

  such that

  forAll i,j,k: S. R((i,R((j,k)))) = R((R((i,j)),k))


Result::

  letting R be
        function((1, 1) --> 1, (1, 2) --> 1, (1, 3) --> 1, (1, 4) --> 1,
                 (2, 1) --> 1, (2, 2) --> 1, (2, 3) --> 1, (2, 4) --> 1,
                 (3, 1) --> 1, (3, 2) --> 1, (3, 3) --> 1, (3, 4) --> 1,
                 (4, 1) --> 1, (4, 2) --> 1, (4, 3) --> 1, (4, 4) --> 1)


The first result is still the same, but there are fewer solutions to be found now - only 3,492. Is this correct? It's always good to check. This number was first published in 1955, by George E. Forsythe, in his paper "SWAC Computes 126 Distinct Semigroups of Order 4". Where does the number 126 come from? This small number comes from ignoring cases where the semigroup is the same except for rearranging the numbers 1,2,3,4. The number we found, 3,492, is found in the paper.

Monoids
~~~~~~~

Let's move further to monoids. A monoid is a semigroup with an extra condition, there has to exist some element of the semigroup, which we will call `e`, which acts as an **identity**. An **identity** is an element such that for all ``i`` in ``S``, ``R(e,i) = R(i,e) = i``.

Firstly we will add a variable to store the value of this ``e``, and then add the extra constraint which makes it an identity:

.. code-block:: essence

  given n : int

  letting S be domain int(1..n)

  find R : function (total) (S,S) --> S

  find e : S

  such that

  forAll i,j,k: S. R((i,R((j,k)))) = R((R((i,j)),k)),
  forAll i : S. M((e,i)) = i /\ M((i,e)) = i,



Result::

  letting R be
        function((1, 1) --> 1, (1, 2) --> 1, (1, 3) --> 1, (1, 4) --> 1,
                 (2, 1) --> 1, (2, 2) --> 1, (2, 3) --> 1, (2, 4) --> 2,
                 (3, 1) --> 1, (3, 2) --> 1, (3, 3) --> 1, (3, 4) --> 3,
                 (4, 1) --> 1, (4, 2) --> 2, (4, 3) --> 3, (4, 4) --> 4)
  letting e be 4

We now have only 624 solutions! We can check this by looking at the amazing online encyclopedia of integer sequences https://oeis.org/A058153 , which tells us there are indeed 624 "labelled monoids" of order n.

Groups
~~~~~~

Finally, let us move to groups. Groups add one important requirement, the concept of an **inverse**. Given some ``i`` in ``S``, ``j`` is an inverse of ``i`` if ``R((i,j)) = R((j,i)) = e``, where ``e`` is our already existing identity.

We will store the inverses as an extra array, and then add this final constraint:

.. code-block:: essence

  given n : int

  letting S be domain int(1..n)

  find R : function (total) (S,S) --> S

  find e : S

  find inv: function S --> S

  such that

  forAll i,j,k: S. R((i,R((j,k)))) = R((R((i,j)),k)),
  forAll i : S. R((e,i)) = i /\ R((i,e)) = i,

  forAll i : S. R((i,inv(i))) = e /\ R((inv(i),i)) = e

Result::

  letting R be
        function((1, 1) --> 1, (1, 2) --> 2, (1, 3) --> 3, (1, 4) --> 4,
                 (2, 1) --> 2, (2, 2) --> 1, (2, 3) --> 4, (2, 4) --> 3,
                 (3, 1) --> 3, (3, 2) --> 4, (3, 3) --> 1, (3, 4) --> 2,
                 (4, 1) --> 4, (4, 2) --> 3, (4, 3) --> 2, (4, 4) --> 1)
  letting e be 4
  letting inv be function(1 --> 1, 2 --> 2, 3 --> 3, 4 --> 4)

This solution has much more going on than our previous ones! For example, each row and column contains the numbers from ``1`` to ``4``, in some order. This (and many, many other results) are true for all groups (but we won't prove this here!). This problem only has 16 solutions, and once we removed the groups which are made by just swapping around 1,2,3 and 4, we would find there was only 2 groups! The extra structure means there are only a small number of groups for each size, compared to the number of semigroups and monoids.

There are many special types of groups; we will consider just one here, **abelian** groups. A group is **abelian** if for all ``i`` and ``j`` in ``S``, ``R((i,j)) = R((j,i))``. Let's add this condition!

.. code-block:: essence

  given n : int

  letting S be domain int(1..n)

  find R : function (total) (S,S) --> S

  find e : S

  find inv: function S --> S

  such that

  forAll i,j,k: S. R((i,R((j,k)))) = R((R((i,j)),k)),
  forAll i : S. R((e,i)) = i /\ R((i,e)) = i,
  forAll i : S. R((i,inv(i))) = e /\ R((inv(i),i)) = e,
  forAll i,j : S. R((i,j)) = R((j,i))


Result::

  letting R be
        function((1, 1) --> 1, (1, 2) --> 2, (1, 3) --> 3, (1, 4) --> 4,
                 (2, 1) --> 2, (2, 2) --> 1, (2, 3) --> 4, (2, 4) --> 3,
                 (3, 1) --> 3, (3, 2) --> 4, (3, 3) --> 1, (3, 4) --> 2,
                 (4, 1) --> 4, (4, 2) --> 3, (4, 3) --> 2, (4, 4) --> 1)
  letting e be 4
  letting inv be function(1 --> 1, 2 --> 2, 3 --> 3, 4 --> 4)

This gives us the same first solution. In fact, there is the same number of solutions (16) to this problem as the previous one, proving that all groups of size 4 are abelian! In fact, the smallest non-abelian group is size 60, and that is beyond the size of problems we can find all solutions to with our current, simple model.
