
.. _demonstrations:

Demonstrations
==============

(In preparation)

We illustrate the use of Conjure to enumerate all labelled connected graphs.

First, we need to decide how to represent graphs.
The simplest representation is to list the tuples.
The usual way to do that is to represent each edge as a set of two distinct vertices.

.. code-block:: essence

   language Essence 1.3
   letting n be 4
   letting G be {{1,2},{2,3},{3,4}}

Each snippet of Essence code can contain a declaration of which dialect of Essence it is written in.
The current version of Essence is 1.3 so we will use that for all these examples.

In this snippet, we declare two aliases.
The number of vertices n is first defined as 4.
Then G is defined as a set of edges.

This snippet is saved in a file ``path-4.param`` that we refer to later.
We should also have a different graph that is not connected:

.. code-block:: essence

   language Essence 1.3
   letting n be 4
   letting G be {{1,2},{4,3}}

which is saved in file ``disconnected-4.param``.

We now need to express what it means for a graph to be connected.

.. code-block:: essence

   language Essence 1.3
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

   language Essence 1.3
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

   language Essence 1.3
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

This model is the fastest yet, but it does generate large intermediate models with many variables.

Finally, there is a simple model ``gc4.essence`` that is faster still than any of the three previous ones.
This model relies on the fact that a graph is disconnected if, and only if, its vertices can be partitioned into two sets, with no edges between vertices in the two different sets.
Here C is used to indicate the elements in a connected component.
There are three constraints.
The first is that C must contain some vertex.
The second is that C must be a connected component; each vertex in C is connected to some other vertex in C (unless C only contains a single vertex).
The third is that the value of ``connected`` is determined by whether it is possible to find some vertex that is not in C.

.. code-block:: essence

   language Essence 1.3
   given n : int(1..)
   letting vertices be domain int(1..n)
   given G : set of set (size 2) of vertices
   find C : matrix indexed by [vertices] of bool
   find connected : bool
   such that
     exists u : vertices . C[u],
     forAll u,v : vertices . ({u,v} in G) -> (C[u] = C[v]),
     (!connected) <-> (exists u : vertices . !C[u])

It should be clear from these four example models that the process of modelling requires careful thought and that the choices made may drastically affect the performance of the solver.
(It is actually possible to improve performance yet more, using relations instead of sets.
This may change in future as the Conjure/Savile Row toolchain is improved.)

We now have four models that determine whether a graph is connected, and the last one seems best.
Let's now use this last model to enumerate the connected graphs over vertices ``{1,2,3,4}``.

.. code-block:: essence

   language Essence 1.3
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

This model has 2**(2n) solutions.
That's it!

