


BIBD
----
- Demonstrate quantifier
This tutorial discusses a classic constraint problem and introduces the use of quantifiers in Essence.
- Describe the problem
The Problem
Balanced Incomplete Block Design (BIBD) is a problem from the field of experimental design. It is best explained with an example.

Emily wants to establish which crops (🥔,🌽,🥦,🥕,🥒, 🍅) grow best in a ***new environment/ fertiliser/something***. She has recruited 4 farmers who are happy to help by growing some of the crops. Unfortunately none of the farmers have enough space to grow every crop, they can each grow 3 different crops. Emily is concerned that the different environment of each farm may impact the crops growth. Therefore she wants to make sure that each farmer grows a different combination of crops and that every crop has been grown in the same number of different farms. This approach is called Balanced Incomplete Block Design (BIBD).

We can build a model to tell us the crops that each farm should grow.

The Model
~~~~~~~~~~~~~~
We need to specify the crops, the number of farms, the number of crops that can be grown per farm, the number of different farms that will grow each crop and the number of crops each pair of farmers has in common.

Emily has decided that she wants each crop to be grown in 2 different farms, and that each pair of farmers will have 1 crop in common.

Essence will take a .param file containing the values of the initial parameters.

.. code-block:: essence

  letting crops be new type enum

- Build a model (step by step)
- Make model better!
