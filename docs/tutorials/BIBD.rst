
BIBD
----

Authors: Chris Jefferson and Alice Lynch

This tutorial discusses a classic constraint problem and introduces the use of quantifiers in Essence.

The Problem
~~~~~~~~~~~

Balanced Incomplete Block Design (BIBD) is a problem from the field of experimental design. It is best explained with an example.

Emily wants to establish which crops (🥔, 🌽, 🥦, 🥕, 🥒, 🍅) grow best in Scotland. She has recruited 4 farmers who are happy to help by growing some of the crops. Unfortunately none of the farmers have enough space to grow every crop, they can each grow 3 different crops. Emily is concerned that the different environment of each farm may impact the crops growth. Therefore she wants to make sure that each farmer grows a different combination of crops and that every crop has been grown in the same number of different farms. This approach is called Balanced Incomplete Block Design (BIBD).

We can build a model to tell us the crops that each farm should grow.

The Model
~~~~~~~~~~~~~~
We need to specify the crops, the number of farms, the number of crops that can be grown per farm, the number of different farms that will grow each crop and the number of crops each pair of farmers has in common.

Emily has decided that she wants each crop to be grown in 2 different farms, and that each pair of farmers will have 1 crop in common.

Essence will take a ``.param`` file containing the values of the initial parameters. In the ``.param`` file we should define the parameters:

.. code-block:: essence

  letting crops be new type enum {🥔, 🌽, 🥦, 🥕, 🥒, 🍅}
  letting farms be 4
  letting crops_per_farm be 3
  letting farms_per_crop be 2
  letting overlap be 1

The model will be in a ``.essence`` file. It should start by accessing the provided parameters, this uses the ``given`` keyword, followed by the names of the parameters and their type.

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops new type enum

Next, we need to define what we are looking for. The ``find`` keyword indicates that the solver should find a value for that variable. We want to find a set containing sets of crops. Each set of crops is a crop assignment for a farm.

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops new type enum

  find crop_assignment: set of set of crops

Once the parameters and decision variables of the model have been defined, we should define the constraints. ``such that`` indicates the start of the constraints.

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops new type enum

  find crop_assignment: set of set of crops

  such that

Result::

  {}

With no constraints it produces an empty set for crop assignment.

The first, basic, constraint is the number of farms. The number of sets in the ``crop_assignment`` set should equal the number of farms. ``|crop_assignment|`` indicates the size of the ``crop_assignment`` set. By setting the size equal to the number of farms (after the ``such that`` keyword) the solver will only produce solutions where the size of the set is the same as the number of farms.  A comma separates constraints, so at the end of a line this indicates that there are more constraints to follow (none for the moment).

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops new type enum

  find crop_assignment: set of set of crops

  such that

  |crop_assignment| = farms,

Result::

  {{},
   {🥒},
   {🥒, 🍅},
   {🍅}}

The model now produces four 'farms' but the number of crops assigned to each are not suitable.

Next we want to apply the number of crops per farm constraint to every set in the crop assignment set. The ``forAll`` keyword will apply the constraint (``|farm| = crops_per_farm``) across every element in the crop_assignment set (represented by ``farm``). The ``.`` separates the constraint from the quantifier setup.

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops new type enum

  find crop_assignment: set of set of crops

  such that

  |crop_assignment| = farms,
  forAll farm in crop_assignment . |farm| = crops_per_farm,


Result::

  {{🥦, 🥕, 🥒},
   {🥦, 🥕, 🍅},
   {🥦, 🥒, 🍅},
   {🥕, 🥒, 🍅}}

The model now has the correct number of farms and assigns the correct number of crops per farm, but doesn't assign all types of crops.

The next constraint is number of farms with a given crop. This is more complex than the previous constraints. Let's go over it step by step.
For every crop we need to find the number of farms assigned that crop and set it to equal the parameter Emily chose for farms per crop. In order to find this we first use a ``forAll`` to apply the constraint to every crop. ``forAll crop : crops . [OurCalculation] = farms_per_crop``

Then we need to count every farm that is planting that crop. For this we should use the ``sum`` quantifier rather than the ``forAll`` (``sum farm in crop_assignment . [Action]``). ``sum`` will add together all the results of the chosen action. In order to use sum to count the number of farms that contain a crop we need to return 1 if the farm is planting the crop and 0 otherwise. The ``in`` keyword can be used to check if a crop is present in a farm, the resulting boolean can be converted to 1 or 0 using ``toInt``.

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops new type enum

  find crop_assignment: set of set of crops

  such that

  |crop_assignment| = farms,
  forAll farm in crop_assignment . |farm| = crops_per_farm,
  forAll crop : crops . (sum farm in crop_assignment . toInt(crop in farm)) = farms_per_crop,

Result::

  {{🥔, 🥕, 🍅},
   {🥔, 🥒, 🍅},
   {🌽, 🥦, 🥕},
   {🌽, 🥦, 🥒}}

Our model now produces a crop assignment that assigns the correct number of crops to each farmer and the correct number of crops in total but there is lot of overlap between the first and second farmer and between the third and fourth farmer but very little overlap between the two pairs. This is why Emily specified the overlap constraint (sometimes called lambda in BIBD models). In order to make sure that every pair of farmers have at least 1 crop in common we need to define another constraint.

We need to check every pair of farms, we can do this by using two ``forAll`` keywords (``forAll farm1 in crop_assignment. forAll farm2 in crop_assignment . [OurConstraint]``). We can then use the ``intersect`` keyword to get all crops that the two farms have in common, and require the size of this intersection to be equal to the overlap parameter (``|farm1 intersect farm2| = overlap``).

However, running the model at this point produces no solutions, as iterating over the ``crop_assignment`` in this way means that sometimes ``farm1`` and ``farm2`` will be the same farm, so the intersection will be the number of crops assigned to the farm (3) and never be 1 (the overlap parameter), resulting in no valid solutions.

In order to avoid this we need to add a further condition to the constraint which checks they are not the same farm before applying the constraint. ``->`` is used, where the left hand side has a condition and the right hand side has a constraint which is only used if the left hand side is true. ``farm1 != farm2 -> |farm1 intersect farm2| = overlap``


.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops new type enum

  find crop_assignment: set of set of crops

  such that

  |crop_assignment| = farms,
  forAll farm in crop_assignment . |farm| = crops_per_farm,
  forAll crop : crops . (sum farm in crop_assignment . toInt(crop in farm)) = farms_per_crop,
  forAll farm1 in crop_assignment. forAll farm2 in crop_assignment . farm1 != farm2 -> |farm1 intersect farm2| = overlap

Result::

  {{🥔, 🥦, 🍅},
   {🥔, 🥕, 🥒},
   {🌽, 🥦, 🥒},
   {🌽, 🥕, 🍅}}

This model produces a valid solution!

Improvements
~~~~~~~~~~~~~~~~~~
Our model now works and produces a correct solution but the code could be improved in places.

There is a nicer way to do the final constraint, instead of using a second ``forAll`` we can use ``{farm1, farm2}`` and ``subsetEq`` to generate all pairs that can be made up from a given set.

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops new type enum

  find crop_assignment: set of set of crops

  such that

  |crop_assignment| = farms,
  forAll farm in crop_assignment . |farm| = crops_per_farm,
  forAll crop : crops . (sum farm in crop_assignment . toInt(crop in farm)) = farms_per_crop,
  forAll {farm1, farm2} subsetEq crop_assignment . |farm1 intersect farm2| = overlap



Providing information in the ``find`` statements rather than as constraints often leads to better performance. Essence provides domain attributes which can be attached to ``find`` statements . One of them is ``size k``, which tells Essence that a set is of size ``k``. In our model the number of farms and the number of crops per farm are in effect the size of the ``crop_assignment`` set and the size of the sets within the ``crop_assignment`` set. Therefore we can move these definitions out of the list of constraints and into the ``find`` statement.

.. code-block:: essence

  given farms, crops_per_farm, farms_per_crop, overlap: int
  given crops new type enum

  find crop_assignment: set (size farms) of set (size crops_per_farm) of crops

  such that
  forAll crop : crops . (sum farm in crop_assignment . toInt(crop in farm)) = farms_per_crop,
  forAll {farm1, farm2} subsetEq crop_assignment . |farm1 intersect farm2| = overlap
