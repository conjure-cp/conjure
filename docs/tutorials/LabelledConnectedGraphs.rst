
Labelled connected graphs
-------------------------

Author: András Salamon (with thanks to Roy Dyckhoff for proofreading)

We illustrate the use of Conjure to enumerate all labelled connected graphs.
The number of labelled connected graphs over a fixed set of ``n`` distinct labels grows quickly; this is `OEIS sequence A001187 <http://oeis.org/A001187>`_.

We first need to decide how to represent graphs.
A standard representation is to list the edges.
One natural representation for each edge is as a set of two distinct vertices.
Vertices of the graph are labelled with integers between 1 and ``n``, and each vertex is regarded as part of the graph, whether there is some edge involving that vertex or not.

.. code-block:: essence

   letting n be 4
   letting G be {{1,2},{2,3},{3,4}}

In this specification, we declare two aliases.
The number of vertices ``n`` is first defined as 4.
Then ``G`` is defined as a set of edges.

This specification is saved in a file ``path-4.param`` that we refer to later.
We should also have a different graph that is not connected:

.. code-block:: essence

   letting n be 4
   letting G be {{1,2},{4,3}}

which is saved in file ``disconnected-4.param``.

We now need to express what it means for a graph to be connected.


Model 1: distance matrix
~~~~~~~~~~~~~~~~~~~~~~~~

In our first attempt, we use a matrix of distances.
Each entry ``reach[u,v]`` represents the length of a shortest path from ``u`` to ``v``, or ``n`` if there is no path from ``u`` to ``v``.
To enforce this property, we use several constraints, one for each possible length; there are four ranges of values we need to cover.
A distance of 0 happens when ``u`` and ``v`` are the same vertex.
A distance of 1 happens when there is an edge from ``u`` to ``v``.
When the distance is greater than 1 but less than ``n``, then there must be some vertex that is a neighbour of ``u`` from which ``v`` is reachable in one less step.
Finally, the distance of ``n`` is used when no neighbour of ``u`` can reach ``v`` (and in this case, the neighbours all have distance of ``n`` to ``v`` as well).

.. code-block:: essence

   given n : int(1..)
   letting vertices be domain int(1..n)
   given G : set of set (size 2) of vertices
   find reach : matrix indexed by [vertices, vertices] of int(0..n)
   such that
     forAll u,v : vertices .
        ((reach[u,v] = 0) -> (u=v))
     /\ ((reach[u,v] = 1) -> ({u,v} in G))
     /\ (((reach[u,v] > 1) \/ (reach[u,v] < n)) ->
         (exists w : vertices . ({u,w} in G) /\ (reach[w,v] = reach[u,v] - 1)))
     /\ ((reach[u,v] = n) -> (forAll w : vertices . !({u,w} in G) \/ (reach[w,v] = n)))
   find connected : bool
   such that
     connected = (forAll u,v : vertices . reach[u,v] < n)

This is stored in file ``gc1.essence``.
The values of ``n`` and ``G`` will be specified later as parameters, such as via the ``path-4.param`` or ``disconnected-4.param`` files.

In the model, first the matrix ``reach`` is specified by imposing the four conditions that we mentioned.
Finally a Boolean variable ``connected`` is used to conveniently indicate whether the ``reach`` matrix represents a connected graph or not; in a connected graph every vertex is reachable from every other vertex.

Let's now try this model with the two graphs defined so far.

.. code-block:: bash

    conjure solve -ac gc1.essence path-4.param
    conjure solve -ac gc1.essence disconnected-4.param

In the solutions found by Conjure, the matrix ``reach`` indicates the distances between each pair of vertices.
In the solution for the connected graph ``gc1-path-4.solution`` all entries are at most 3.

.. code-block:: essence

   letting connected be true
   letting reach be
     [[0, 1, 2, 3; int(1..4)], [1, 0, 1, 2; int(1..4)],
      [2, 1, 0, 1; int(1..4)], [3, 2, 1, 0; int(1..4)]; int(1..4)]
   $ Visualisation for reach
   $ 0 1 2 3
   $ 1 0 1 2
   $ 2 1 0 1
   $ 3 2 1 0

In contrast, in the solution for the disconnected graph ``gc1-disconnected-4.solution`` there are some entries that are 4:

.. code-block:: essence

   letting connected be false
   letting reach be
     [[0, 1, 4, 4; int(1..4)], [1, 0, 4, 4; int(1..4)],
      [4, 4, 0, 1; int(1..4)], [4, 4, 1, 0; int(1..4)]; int(1..4)]
   $ Visualisation for reach
   $ 0 1 4 4
   $ 1 0 4 4
   $ 4 4 0 1
   $ 4 4 1 0

Graphs with four vertices are good for quick testing but are too small to notice much difference between models.
Small differences are important for tasks such as enumerating many objects, when even a small difference is multiplied by the number of objects.
For testing we can create other parameter files containing graphs with more vertices.
Notice that we do not have to change the model, only the parameter files containing the input data.

Testing with larger graphs of say 1000 vertices, it becomes clear that this first model works but does not scale well.
It computes the lengths of the shortest paths between pairs of vertices, from which we can deduce whether the graph is connected.
This is quite round-about!
We can now try to improve the model by asking the system to do less work.
After all, we don't actually need all the pairwise distances.


Model 2: reachability matrix
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the following model, stored as file ``gc2.essence``, the reachability matrix uses Boolean values for the distances rather than integers, with ``true`` representing reachable and ``false`` unreachable.
Each entry ``reach[u,v]`` represents whether it is possible to reach ``v`` by some path that starts at ``u``.
This is modelled as the disjunction of three conditions: ``u`` is reachable from itself, any neighbour of ``u`` is reachable from it, and if ``v`` is not a neighbour of ``u`` then there should be a neighbour ``w`` of ``u`` so that ``v`` is reachable from ``w``.

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

In the solutions found by Conjure, the reachability matrix contains regions of true entries indicating the connected components.

In the connected graph all entries are true:

.. code-block:: essence

   letting connected be true
   letting reach be
     [[true, true, true, true; int(1..4)], [true, true, true, true; int(1..4)],
      [true, true, true, true; int(1..4)], [true, true, true, true; int(1..4)];
      int(1..4)]
   $ Visualisation for reach
   $ T T T T
   $ T T T T
   $ T T T T
   $ T T T T

In contrast, in the disconnected graph there are some false entries:

.. code-block:: essence

   letting connected be false
   letting reach be
     [[true, true, false, false; int(1..4)], [true, true, false, false; int(1..4)],
      [false, false, true, true; int(1..4)], [false, false, true, true; int(1..4)];
      int(1..4)]
   $ Visualisation for reach
   $ T T _ _
   $ T T _ _
   $ _ _ T T
   $ _ _ T T

This model takes about half as long as the previous one, but is still rather slow for large graphs.
Moreover, this model fails to capture all the constraints: it will always allow the all-true reachability matrix as one solution, and so it will potentially always consider every graph to be connected.
The reader is encouraged to think about this flaw in the model.


Model 3: structured reachability matrices
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

In the previous two models the solver may spend a long time early in the search process looking for ways to reach vertices that are far away, even though it would be more efficient to focus the early stages of search on vertices close by.
It is possible to improve performance by guiding the search to consider nearby vertices before vertices that are far from each other.
The following model ``gc3.essence`` uses additional decision variables to more precisely control how the desired reachability matrix should be computed.
There are multiple reachability matrices.
Each corresponds to a specific maximum distance.
The first ``n`` by ``n`` matrix ``reach[0]`` expresses reachability in one step, and is simply the adjacency matrix of the graph.
The entry ``reach[k,u,v]`` expresses whether ``v`` is reachable from ``u`` via a path of length at most ``2**k``.
If a vertex ``v`` is reachable from some vertex ``u``, then it can be reached in at most ``n-1`` steps.
(Note: in this model a vertex cannot reach itself in zero steps, so a graph with a single vertex is not regarded as connected.)

.. code-block:: essence

   given n : int(1..)
   letting vertices be domain int(1..n)
   given G : set of set (size 2) of vertices
   letting m be sum([1 | i : int(0..64), 2**i <= n])
   find reach : matrix indexed by [int(0..m), vertices, vertices] of bool
   such that
     forAll u,v : vertices . reach[0,u,v] = ({u,v} in G),
     forAll i : int(0..(m-1)) . forAll u,v : vertices . reach[i+1,u,v] =
       (reach[i,u,v] \/ (exists w : vertices . (reach[i,u,w] /\ reach[i,w,v]))),
   find connected : bool
   such that
     connected = (forAll u,v : vertices . reach[m,u,v])

The variable ``m`` is used to compute the number of matrices that are required; this is the smallest integer that is not less than the base-2 logarithm of ``n``.
(This is computed by discrete integration as Essence currently does not have a logarithm operator.)
The value of ``connected`` is then based on whether whether ``reach[m]`` contains any false entries.

This model performs well for small graphs, but it generates intermediate distance matrices, each containing ``n**2`` variables.
For large graphs the number of intermediate variables used to represent these intermediate distance matrices grows quickly, which causes Savile Row to run out of memory.
We omit the solutions here.
The sequence of solutions shows how the number of true values in the reachability matrices increases, until reaching a fixed point.


Model 4: connected component
~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Each of the three models so far deals with all possible pairs of vertices.
The number of possible pairs of vertices is quadratic in the number of vertices.
However, many graphs are sparse, with a number of edges that is bounded by a linear function of the number of vertices.
For sparse graphs, and especially those with many vertices, it is therefore important to only consider the edges that are present rather than all possible pairs of vertices.
The next model ``gc4.essence`` uses this insight, and is indeed faster than any of the three previous ones.

The model builds on the fact that a graph is disconnected if, and only if, its vertices can be partitioned into two sets, with no edges between vertices in the two different sets.
Here ``C`` is used to indicate a subset of the vertices.
There are three constraints.
The first is that ``C`` must contain some vertex.
The second is that ``C`` must be a connected component; each vertex in ``C`` is connected to some other vertex in ``C`` (unless ``C`` only contains a single vertex).
The third is that the value of ``connected`` is determined by whether it is possible to find some vertex that is not in ``C``.
The following is an attempt to capture these constraints in an Essence specification.

.. code-block:: essence

   given n : int(1..)
   letting vertices be domain int(1..n)
   given G : set of set (size 2) of vertices
   find C : set of vertices
   find connected : bool
   such that
     exists u : vertices . u in C,
     forAll e in G . (min(e) in C) = (max(e) in C),
     connected = !(exists u : vertices . !(u in C))

This is the solution for ``disconnected-4.param``:

.. code-block:: essence

   letting C be {1, 2}
   letting connected be false

Model ``gc4.essence`` yields a solution quickly.
Unfortunately, like Model 2 above, it can also give incorrect results: letting ``C`` be the set of all vertices and letting ``connected`` be true is always a solution, whether the graph is connected or not.
This can be confirmed by asking Conjure to generate all solutions:

.. code-block:: essence

   conjure solve -ac --number-of-solutions=all gc4.essence

This gives two solutions, the one above and the following one:

.. code-block:: essence

   letting C be {1, 2, 3, 4}
   letting connected be true

It is actually possible to ensure that this "solution" is never the first one generated, and then to ask Conjure to only look for the first solution; if the graph is not connected then the first solution will correctly indicate its status.
However, this relies on precise knowledge of the ordering heuristics being employed at each stage of the toolchain.

The problem with this fourth specification is that it only captures the property that ``C`` is a union of connected components.
We would need to add additional constraints to enforce the property that C should contain only one connected component.
This can be done, but is not especially efficent.

For the mathematically minded, the problem is that connectivity is not expressible in monadic existential second order logic, forcing us to express the problem differently, such as in non-monadic existential or monadic universal second order logic, or in first-order logic with fixed points.
The first two approaches tend to increase the search effort, while Essence lacks fixed point operators so we can't use the third approach.


Model 5: minimal connected component
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

Let's look for a robust approach that won't unexpectedly fail if parts of the toolchain change which optimisations they perform or the order in which evaluations occur.

One option could be to look for solutions of a more restrictive model which includes an additional constraint that requires some vertex to not be in ``C``.
This model would have a solution precisely if the graph is *not* connected.
Failure to find solutions to this model would then indicate connectivity.
It is possible to call Conjure from a script that uses the failure to find solutions to conclude connectivity, but the Conjure toolchain currently does not support testing for the presence of solutions directly.

In place of such an "if-has-solution" directive (which is not currently supported), we could instead quantify over all possible subsets of vertices.
Such an approach quickly becomes infeasible as ``n`` grows (and is much worse than the models considered so far), because it attempts to check ``2**n`` subsets.

As another option, we can make use of the optimisation features of Essence to find a solution with a ``C`` of minimal cardinality.
This ensures that ``C`` can only contain one connected component.
Choosing a minimal ``C`` ensures that when there is more than one solution, then the one that is generated always indicates the failure of connectivity.
Since we don't care about the minimal ``C``, as long as it is smaller than the set of all vertices if possible, we also replace the general requirement for non-emptiness by a constraint that always forces the set ``C`` to contain the vertex labelled 1.

.. code-block:: essence

   given n : int(1..)
   letting vertices be domain int(1..n)
   given G : set of set (size 2) of vertices
   find C : set of vertices
   find connected : bool
   such that
     1 in C,
     forAll e in G . (min(e) in C) = (max(e) in C)
   minimising |C|

This model ``gc5.essence`` is still straightforward, even with the additional complication to rule out incorrect solutions.
Out of the correct models so far, this tends to generate the smallest input files for the back-end constraint or SAT solver, and also tends to be the fastest.
Moreover, it scales to large sparse graphs as it avoids quadratic growth in the number of intermediate variables.


Generating all connected graphs
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~

We now have a fast model for graph connectivity.
Let's modify it as ``gce1.essence``, hardcoding n to be 4 and asking the solver to find G as well as C.

.. code-block:: essence

   letting n be 4
   letting vertices be domain int(1..n)
   find G : set of set (size 2) of vertices
   find C : set of vertices
   such that
     1 in C,
     forAll e in G . (min(e) in C) = (max(e) in C)
   minimising |C|

We now ask for all solutions:

.. code-block:: bash

    conjure solve -ac --number-of-solutions=all gce1.essence

However, this finds only one solution!

The solver finds one solution that minimises ``|C|``; this minimisation is performed globally over all possible solutions.
This is what we intended when ``G`` was given, but is not what we want if our goal is to generate *all* connected graphs.
We want to minimise ``C`` for each choice of ``G``, producing one solution for each ``G``.
There is no way to tell Conjure that minimisation should be restricted to the decision variable ``C``.

Checking whether there is a nontrivial connected component seems to be the most efficient model for graph connectivity, but it doesn't work in the setting of generating all connected graphs.
We therefore need to choose one of the other models to start with, say the iterated adjacency matrix representation.

We now use this model of connectivity to enumerate the labelled connected graphs over the vertices ``{1,2,3,4}``.
Previously we checked connectivity of a given graph ``G``.
We now instead ask the solver to find ``G``, specifying that it be connected.
We do this by asking for the same adjacency matrix ``reach`` as before, but in addition asking for the graph ``G``.
We also hardcode ``n``, so no parameter file is needed, and add the condition that previously determined the value of the ``connected`` decision variable as a constraint.

.. code-block:: essence

   letting n be 4
   letting vertices be domain int(1..n)
   find G : set of set (size 2) of vertices
   letting m be sum([1 | i : int(0..64), 2**i <= n])
   find reach : matrix indexed by [int(0..m), vertices, vertices] of bool
   such that
     forAll u,v : vertices . reach[0,u,v] = ({u,v} in G),
     forAll i : int(0..(m-1)) . forAll u,v : vertices . reach[i+1,u,v] =
       (reach[i,u,v] \/ (exists w : vertices . (reach[i,u,w] /\ reach[i,w,v]))),
     forAll u,v : vertices . reach[m,u,v]

If this model is in the file ``gce2.essence``, then we now need to explicitly ask Conjure to generate all the possible graphs:

.. code-block:: bash

    conjure solve -ac --number-of-solutions=all gce2.essence

In this case Conjure generates 38 solutions, one solution per file.

Instead of listing the edges of a graph, and then deriving the adjacency matrix as necessary, it is also possible to use the adjacency matrix representation.
As an exercise, modify the models of connectivity to use the adjacency matrix representation instead of the set of edges representation.


