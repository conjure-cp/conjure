
.. _demonstrations:

Demonstrations
==============


We demonstrate the use of Conjure for some small problems.


Number puzzle
-------------

We first show how to solve a classic `word addition <https://en.wikipedia.org/wiki/Verbal_arithmetic>`_ puzzle, due to Dudeney :cite:`dudeney1924puzzle`.
::
        SEND
     +  MORE
     -------
     = MONEY

Here each letter represents a numeric digit in an addition, and we are asked to find an assignment of digits to letters so that the number represented by the digits SEND when added to the number represented by MORE yields the number represented by MONEY.

.. code-block:: essence

   language Essence 1.3
   letting letters be new type enum {S,E,N,D,M,O,R,Y}
   find f : function (injective) letters --> int(0..9)
   find carry1,carry2,carry3,carry4 : int(0..2)
   such that
              f(D) + f(E) = f(Y) + 10*carry1,
     carry1 + f(N) + f(R) = f(E) + 10*carry2,
     carry2 + f(E) + f(O) = f(N) + 10*carry3,
     carry3 + f(S) + f(M) = f(O) + 10*carry4,
     carry4 = f(M)

Each snippet of Essence code can optionally contain a declaration of which dialect of Essence it is written in.
The current version of Essence is 1.3.
We leave out this declaration in the remaining examples to avoid repetition.

This model is stored in ``sm1.essence``; let's use Conjure to find the solution:

.. code-block:: bash

    conjure solve -ac sm1.essence

Unless we specify what to call the solution, it is saved as ``sm1.solution``.

.. code-block:: essence

   letting carry1 be 1
   letting carry2 be 0
   letting carry3 be 1
   letting carry4 be 0
   letting f be function(S --> 2, E --> 8, N --> 1, D --> 7, M --> 0, O --> 3, R --> 6, Y --> 5)

This looks odd: we usually don't allow a number to begin with a zero digit, but the solution maps M to 0.
Let's add the missing constraints to file ``sm2.essence``:

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

   conjure solve -ac sm2.essence

This now leads to the solution we expected:

.. code-block:: essence

   letting carry1 be 1
   letting carry2 be 1
   letting carry3 be 0
   letting carry4 be 1
   letting f be function(S --> 9, E --> 5, N --> 6, D --> 7, M --> 1, O --> 0, R --> 8, Y --> 2)

The solution includes both the mapping we were looking for, as well as the values for the carry digits that were needed to find the solution.


Labelled connected graphs
-------------------------

We first illustrate the use of Conjure to enumerate all labelled connected graphs.
There are 2**(2n) labelled connected graphs over a fixed set of n distinct labels.

First, we need to decide how to represent graphs.
The simplest representation is to list the tuples.
The usual way to do that is to represent each edge as a set of two distinct vertices.

.. code-block:: essence

   letting n be 4
   letting G be {{1,2},{2,3},{3,4}}


In this snippet, we declare two aliases.
The number of vertices n is first defined as 4.
Then G is defined as a set of edges.

This snippet is saved in a file ``path-4.param`` that we refer to later.
We should also have a different graph that is not connected:

.. code-block:: essence

   letting n be 4
   letting G be {{1,2},{4,3}}

which is saved in file ``disconnected-4.param``.

We now need to express what it means for a graph to be connected.

.. code-block:: essence

   given n : int(1..)
   letting vertices be domain int(1..n)
   given G : set of set (size 2) of vertices
   find reach : matrix indexed by [vertices, vertices] of bool
   such that
     forAll u,v : vertices . reach[u,v] =
       ((u = v) \/ ({u,v} in G) \/
       (exists w : vertices . ({u,w} in G) /\ reach[w,v]))
   find connected : bool
   such that
     connected = (forAll u,v : vertices . reach[u,v])

This is stored in file ``gc1.essence``.
Vertices of the graph are labelled with integers between 1 and n, and each vertex is regarded as part of the graph, whether there is some edge involving that vertex or not.
The values of n and G will be specified later as parameters, such as via the ``path-4.param`` or ``disconnected-4.param`` files.
First a matrix ``reach`` is specified.
Each entry ``reach[u,v]`` represents whether it is possible to reach v by some path that starts at u; this is modelled as the disjunction of three conditions: u is reachable from itself, any neighbour of u is reachable from it, and if v is not a neighbour of u then there should be a neighbour w of u so that v is reachable from w.
Finally a Boolean variable is used to indicate whether the ``reach`` matrix represents a connected graph or not; in a connected graph every vertex is reachable from every other vertex.

Let's now try this model with the two graphs defined so far.

.. code-block:: bash

    conjure solve -ac gc1.essence path-4.param
    conjure solve -ac gc1.essence disconnected-4.param

In the solutions found by Conjure, the reachability matrix contains regions of true entries indicating the connected components.
In the connected graph all entries are true.
In contrast, in the disconnected graph there are some false entries.

We can now create other parameter files containing graphs with more vertices.
Graphs with four vertices are good for quick testing but are too small to notice much difference between models.

The first model works but it doesn't scale well.
We can try to improve it.

In the following model, the reachability matrix uses integer values for the distances rather than Boolean values, with the possible values from 0 to n.
A value of n indicates that the two vertices cannot reach each other, and a value of 0 is used for the distance from a vertex to itself.

.. code-block:: essence

   given n : int(1..)
   letting vertices be domain int(1..n)
   given G : set of set (size 2) of vertices
   find reach : matrix indexed by [vertices, vertices] of int(0..n)
   such that
     forAll u,v : vertices .
        ((reach[u,v] = 0) -> (u=v))
     /\ ((reach[u,v] = 1) -> ({u,v} in G))
     /\ (((reach[u,v] > 1) \/ (reach[u,v] < n)) -> (exists w : vertices . ({u,w} in G) /\ (reach[w,v] = reach[u,v] - 1)))
     /\ ((reach[u,v] = n) -> (forAll w : vertices . !({u,w} in G) \/ (reach[w,v] = n)))
   find connected : bool
   such that
     connected = (forAll u,v : vertices . reach[u,v] < n)

Unfortunately, this model ``gc2.essence`` takes about twice as long as the previous one.
On the other hand, it does compute the lengths of the shortest paths between pairs of vertices.

The following model ``gc3.essence`` uses additional decision variables to more precisely control how the reachability matrix should be computed.
There are now multiple reachability matrices.
Each corresponds to a specific maximum distance.
The first matrix ``reach[0]`` expresses reachability in one step.
The entry ``reach[k,u,v]`` expresses whether v is reachable from u via a path of length at most 2**k.

.. code-block:: essence

   given n : int(1..)
   letting vertices be domain int(1..n)
   given G : set of set (size 2) of vertices
   letting m be sum([1 | i : int(0..64), 2**i <= n])
   find reach : matrix indexed by [int(0..m), vertices, vertices] of bool
   such that
     forAll u,v : vertices . reach[0,u,v] = ({u,v} in G),
     forAll i : int(0..(m-1)) . forAll u,v : vertices .
       A[i+1,u,v] = (A[i,u,v] \/ (exists w : vertices . (A[i,u,w]/\A[i,w,v])))
   find connected : bool
   such that
     connected = (forAll u,v : vertices . reach[m,u,v])

This model is the fastest yet, but it generates large intermediate distance matrices containing many variables.

Finally, there is a simple model ``gc4.essence`` that is faster still than any of the three previous ones.
This model relies on the fact that a graph is disconnected if, and only if, its vertices can be partitioned into two sets, with no edges between vertices in the two different sets.
Here C is used to indicate a subset of the vertices.
There are three constraints.
The first is that C must contain some vertex.
The second is that C must be a connected component; each vertex in C is connected to some other vertex in C (unless C only contains a single vertex).
The third is that the value of ``connected`` is determined by whether it is possible to find some vertex that is not in C.

.. code-block:: essence

   given n : int(1..)
   letting vertices be domain int(1..n)
   given G : set of set (size 2) of vertices
   find C : set of vertices
   find connected : bool
   such that
     exists u : vertices . u in C,
     forAll e : G . (min(e) in C) = (max(e) in C),
     connected = !(exists u : vertices . !(u in C))

It should be clear from these four example models that the process of modelling requires careful thought and that the choices made may drastically affect the performance of the solver.
(It is actually possible to improve performance yet more, using relations and matrices instead of sets.
This may change in future as the Conjure/Savile Row toolchain is improved.)

We now have four models that determine whether a graph is connected, and the last one seems best.
Let's now use this last model to enumerate the connected graphs over vertices ``{1,2,3,4}``.

.. code-block:: essence

   letting n be 4
   letting vertices be domain int(1..n)
   find G : set of set (size 2) of vertices
   find C : matrix indexed by [vertices] of bool
   such that
     exists u : vertices . C[u],
     forAll u,v : vertices . ({u,v} in G) -> (C[u] = C[v]),
     !(exists u : vertices . !C[u])

This snippet is in file ``gce.essence``.
Let's run Conjure to generate all the possible graphs.

.. code-block:: bash

    conjure solve -ac --number-of-solutions=all gce.essence

In this case there are 256 solutions.

