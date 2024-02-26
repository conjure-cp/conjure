Handcrafting Instance Generators in Essence
-------------------------------------------

*Authors: Joan Espasa Arxer and Christopher Stone*

In modelling it is common to create an abstract model that expects some input parameters (Also known as "instances") which are required to run and test the model.
In this tutorial we demonstrate how to use ESSENCE to handcraft a generator of instances that can be used to produce input parameters for a specific model.

Instances for the Knapsack problem
~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
Here is the model of the Knapsack Problem from (<link to other tutorial>) - knapsack.essence

.. code-block:: essence

    given number_items : int
    letting items be domain int (1..number_items)
    given weight, gain : function (total) items --> int
    given capacity : int
    find picked : set of items
    maximising sum i in picked . gain(i)
        such that (sum i in picked . weight(i)) <= capacity

This model has 4 different "given" statements :

- number_items: an integer for number of items

- weight: a functions that associates an integer(weight) to each item

- gain: a function that associates an integer(gain) to each item

- capacity: an integer that defines the capacity of the knapsack

The first parameter is fairly simple and we can even write this parameter with some value by hand
by writing on a separate file (which we will call items.param):

.. code-block:: essence
    
    letting number_items be 20

The remaining 3 parameters are more complex and labourious to be defined (too much work to be done by hand!) so we are going to write an ESSENCE specification that will create them for us.
The fundamental starting step is writing find statements for each variable we wish to generate and ensure that the names of the variable (identifiers) are left unchanged. We can do so by creating a new file called generator.essence and write:

.. code-block:: essence

    given number_items : int
    letting items be domain int(1..number_items)
    find weight: function (total) items --> int(1..1000)
    find gain: function (total) items --> int(1..1000)
    find capacity: int(1..5000)

Solving the above model (by running 'conjure solve generator.essence items.param' on the console) will create a set of parameters for our knapsack model. However, these instances are not interesting enough yet.

.. code-block:: essence

    language Essence 1.3
    letting capacity be 1
    letting gain be
            function(1 --> 1, 2 --> 1, 3 --> 1, 4 --> 1, 5 --> 1, 6 --> 1, 7 --> 1, 8 --> 1, 9 --> 1, 10 --> 1, 11 --> 1,
                     12 --> 1, 13 --> 1, 14 --> 1, 15 --> 1, 16 --> 1, 17 --> 1, 18 --> 1, 19 --> 1, 20 --> 1)
    letting weight be
            function(1 --> 1, 2 --> 1, 3 --> 1, 4 --> 1, 5 --> 1, 6 --> 1, 7 --> 1, 8 --> 1, 9 --> 1, 10 --> 1, 11 --> 1,
                     12 --> 1, 13 --> 1, 14 --> 1, 15 --> 1, 16 --> 1, 17 --> 1, 18 --> 1, 19 --> 1, 20 --> 1)
                 
We can make our instances more interesting by adding constraints into our generator's model.
The first thing we notice is that all values assigned are identical, a bit TOO symmetrical for our taste.
One simple solution to this issue is ensuring that all weights and gains assignments are associated with distinct values. This can be done by imposing `injectivity <https://en.wikipedia.org/wiki/Injective_function>`_ as a property of the function.

.. code-block:: essence
    
    find weight: function (total, injective) items --> int(1..1000)
    find gain: function (total, injective) items --> int(1..1000)

And the results would be

.. code-block:: essence

    language Essence 1.3    
    letting capacity be 1
    letting gain be
            function(1 --> 1, 2 --> 2, 3 --> 3, 4 --> 4, 5 --> 5, 6 --> 6, 7 --> 7, 8 --> 8, 9 --> 9, 10 --> 10, 11 --> 11,
                     12 --> 12, 13 --> 13, 14 --> 14, 15 --> 15, 16 --> 16, 17 --> 17, 18 --> 18, 19 --> 19, 20 --> 20)
    letting weight be
            function(1 --> 1, 2 --> 2, 3 --> 3, 4 --> 4, 5 --> 5, 6 --> 6, 7 --> 7, 8 --> 8, 9 --> 9, 10 --> 10, 11 --> 11,
                     12 --> 12, 13 --> 13, 14 --> 14, 15 --> 15, 16 --> 16, 17 --> 17, 18 --> 18, 19 --> 19, 20 --> 20)

This gives us a slighly more interesting parameters set but it is not there yet
The specific order that appears in the results is solver dependent. The default solver used by conjure is Minion and we can use an optional flag to have the variables assigned in a random order. This can be done with this command:

``conjure solve generator.essence items.param --solver-options=-randomiseorder``

Alternatively one can use another solver that uses randomness by default

.. code-block:: essence

    language Essence 1.3
    letting capacity be 2841
    letting gain be
            function(1 --> 858, 2 --> 653, 3 --> 673, 4 --> 365, 5 --> 389, 6 --> 783, 7 --> 566, 8 --> 664, 9 --> 387,
                     10 --> 576, 11 --> 864, 12 --> 741, 13 --> 102, 14 --> 735, 15 --> 276, 16 --> 41, 17 --> 132,
                     18 --> 974, 19 --> 293, 20 --> 381)
    letting weight be
            function(1 --> 946, 2 --> 435, 3 --> 796, 4 --> 653, 5 --> 291, 6 --> 101, 7 --> 924, 8 --> 988, 9 --> 854,
                     10 --> 952, 11 --> 228, 12 --> 189, 13 --> 88, 14 --> 270, 15 --> 868, 16 --> 903, 17 --> 743,
                     18 --> 396, 19 --> 174, 20 --> 446)

Now it is starting to look more like a proper instance. At this point we can add some knowledge about the problem to formulate some constraints that will ensure that the instances are not trivial. ie when the sum of all the weights is smaller than the capacity so we can't put all the objects in the knapsack or when all the objects are heavier  than the capacity so that no object can be picked. Thefore we add constraints such as:

.. code-block:: essence

    such that (sum ([w | (_,w) <- weight]) > capacity*2)

This means that the sum of all the weights should be greater than twice the capacity of the knapsack. From this we can expect that on average no more than half of the objects will fit in the knapsack.
The expression ``[w | (_,w) <- weight]`` is a list `comprehension <https://en.wikipedia.org/wiki/List_comprehension>`_ that extracts all right hand values of the ``weight`` function. The underscore character means we do not care about the left hand side values.
To ensure that the solver does not take it too far we impose an upper bound using a similar constraint. We impose that the sum of the objects weights 5 times the capacity of the knapsack, so we can expect that only between 20% and 50% of the items will fit in the knapsack in each instance.

.. code-block:: essence

    such that (sum ([w | (_,w) <- weight]) < capacity*5)

At this point it will be harder to see specific properties of the instances just by eyeballing the parameters but we can be confident that the properties we have imposed are there.
We can add some more constraints to refine the values of the instances for practice/exercise by enforcing that no object is heavier than a third of the knapsack capacity

.. code-block:: essence

    such that forAll (_,w) in weight .  w < capacity / 3

On top of that we can enfore a constraint on the density of the values in each object by limiting the ratio between the weight and gain of each specific object with:

.. code-block:: essence

    such that forAll element : items .
            gain(element) <= 3*weight(element)

Finally the model of the generator is now : 

.. code-block:: essence

    given number_items : int
    letting items be domain int(1..number_items)
    
    find weight: function (total, injective) items --> int(1..1000)
    find gain: function (total, injective) items --> int(1..1000)
    find capacity: int(1..5000)
    such that (sum ([w | (_,w) <- weight]) > capacity*2)
    such that (sum ([w | (_,w) <- weight]) < capacity*3)
    such that forAll (_,w) in weight .  w < capacity / 3
    such that forAll element : items .
                gain(element) <= 3*weight(element)

After running once again the solver we can take the output solution file `generator-items.solution` and append it to the items.param (by concatenating the files or simply coping the content into it) 
We can finally test our instance by running 
"conjure solve knapsack.essence items.param"

**Tada! your model is being tested on some instance!**

If your computer is powerful enough you can try larger values in "letting number_items be 20" (40-50 items will already produce substantially harder instances)
Like for other forms of modelling writing instance generators is in large part an art. If this is not your kind of thing and you would like a fully automated system that can produce instances you may check out `this method <https://link.springer.com/chapter/10.1007/978-3-030-30048-7_1>`_ [ code available `here <https://github.com/stacs-cp/CP2019-InstanceGen>`_ ]
