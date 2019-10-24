Simple Permutations
-------------------

Authors: Ruth Hoffmann and Gökberk Koçak

Problem
~~~~~~~

Let a permutation be a sequence of length ``n`` consisting of numbers between 1 and ``n``, in any order with each number occurring exactly once.

An interval in a permutation :math:`{\sigma}` is a factor of contiguous values of σ such that their indices are consecutive.
For example, in the permutation :math:`{\pi} = 346978215`, :math:`{\pi}(4){\pi}(5){\pi}(6) = 978` is an interval, whereas :math:`{\pi}(1){\pi}(2){\pi}(3){\pi}(4) = 3469` is not.
It is easy to see that every permutation of length ``n`` has intervals of length 0, 1 and ``n`` , at least. The permutations of length n that **only** contain intervals of length 0, 1 and ``n`` are said to be simple.
So for example the permutation :math:`{\pi} = 346978215` is not simple as we have seen in the example above that it contains an interval, on the other hand :math:`{\sigma} = 526184937` is simple as there are no intervals of length strictly greater than 1, except the whole of :math:`{\sigma}`. 
See :cite:`hoffmann2015thesis` for more information on permutation patterns and simple permutations.

.. code-block:: essence 

    language Essence 1.3
    given n : int
    given s : sequence (size n) of int
    find result : bool 
    such that
        result = and([ max(subs) - min(subs) + 1 != |subs| | i : int(1..n), j : int(1..n), i < j, j - i + 1 < |s|, j - i + 1 > 1, 
            letting subs be [s(k) | k : int(i..j)]])
