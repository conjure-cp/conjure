


BIBD
----

This tutorial discusses a classic constraint problem and introduces the use of quantifiers in Essence.

The Problem
~~~~~~~~~~~

Balanced Incomplete Block Design (BIBD) is a problem from the field of experimental design. It is best explained with an example.

Emily wants to establish which crops (🥔,🌽,🥦,🥕,🥒, 🍅) grow best in Scotland. She has recruited 4 farmers who are happy to help by growing some of the crops. Unfortunately none of the farmers have enough space to grow every crop, they can each grow 3 different crops. Emily is concerned that the different environment of each farm may impact the crops growth. Therefore she wants to make sure that each farmer grows a different combination of crops and that every crop has been grown in the same number of different farms. This approach is called Balanced Incomplete Block Design (BIBD).

We can build a model to tell us the crops that each farm should grow.

The Model
~~~~~~~~~~~~~~
We need to specify the crops, the number of farms, the number of crops that can be grown per farm, the number of different farms that will grow each crop and the number of crops each pair of farmers has in common.

Emily has decided that she wants each crop to be grown in 2 different farms, and that each pair of farmers will have 1 crop in common.

Essence will take a .param file containing the values of the initial parameters. In the .param file we should define the parameters:

.. code-block:: essence

  letting crops be new type enum {🥔,🌽,🥦,🥕,🥒, 🍅}
  letting farms be 4
  letting crops_per_farm be 3
  letting farms_per_crop be 2
  letting overlap be 1

The model will be in a .essence file. It should start by accessing the provided parameters, this uses the given keyword, followed by the names of the parameters and their type.

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops: enum

Next, we need to define what we are looking for. The 'find' keyword indicates that the solver should find a value to for that variable. We want to find a set containing sets of crops. Each set of crops is a crop assignment for a farm.

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops: enum

  find crop_assignment: set of set of crops

Once the parameters and ***targets?** of the model have been defined, we should define the constraints. ``such that`` indicates the start of the constraints.

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops: enum

  find crop_assignment: set of set of crops

  such that

The first, basic, constraints is the number of farms. The number of sets in the crop_assignment set should equal the numbers of farms. ``|crop_assignment|`` indicates the size of the crop_assignment set. By setting the size equal to the number of farms (after the such that keyword) the solver will only produce solutions where the size of the set is the same as the number of farms.  A comma on the end of line indicates that there are more constraints to follow.

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops: enum

  find crop_assignment: set of set of crops

  such that

  |crop_assignment| = farms,

Next we want to apply the number of crops per farm constraint to every set in the crop assignment set. The ``forAll`` keyword will apply the constraint (``|farm| = crops_per_farm``) across every element in the crop_assignment set (represented by ``farm``). The ``.`` separates the constraint from the quantifier setup.

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops: enum

  find crop_assignment: set of set of crops

  such that

  |crop_assignment| = farms,
  forAll farm in crop_assignment . |farm| = crops_per_farm,

The next constraint is number of farms with a given crop. This is more complex than the previous constraints. Let's go over it step by step.
For every crop we need to find the number of farms assigned that crop and set it to equal the parameter Emily chose for farms per crop. In order to find this we first use a ``forAll`` to apply the constraint to every crop. ``forAll crop : crops . [OurCalculation] = farms_per_crop``

Then we need to count every farm that is planting that crop. For this we should use the ``sum`` quantifier rather than the ``forAll`` (``sum farm in crop_assignment . [Action]``). ``sum`` will add together all the results of the chosen action. In order to use sum to count the number of farms that contain a crop we need to return 1 if the farm is planting the crop and 0 otherwise. The ``in`` keyword can be used to check if a crop is present in a farm, the resulting boolean can be converted to 1 or 0 using ``toInt``.

..
  crop in s is true if a given crop is in a given farm
..
  count the number toInt turns it into 1 or 0.
..
  Then sum over all of the farms in crop_assignment

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops: enum

  find crop_assignment: set of set of crops

  such that

  |crop_assignment| = farms,
  forAll farm in crop_assignment . |farm| = crops_per_farm,
  forAll crop : crops . (sum farm in crop_assignment . toInt(crop in farm)) = farms_per_crop,
