
.. _introduction:

Introduction
============

Conjure is an automated constraint modelling tool for Constraint Programming.

Its input language, Essence, is a high level problem specification language.
Essence allows writing problem specifications at a high level of abstraction and without having to make a lot of low level modelling decisions.

Conjure reads in abstract problem specifications (in Essence) and produces concrete constraint programming models (in Essence').
Essence' is a solver independent constraint modelling language.
Using the Savile Row tool, an Essence' model can be instantiated with parameter values and solved using one of several backends.
More information on Savile Row can be found on `its website <http://savilerow.cs.st-andrews.ac.uk>`_.

Conjure works at the problem class level.
A problem class is a parameterised specification of a problem; it does not encode a single problem but a class of problems.
For example, a problem specification for the game of Sudoku is typically parameterised over the hints (the prefilled cells).
A problem specification (or model) at the class level is said to be *instantiated* when values are provided for its parameters.
In the case of a Sudoku, the parameter values are the contents of the hint cells.

Operating at the class level has one very important benefit: Conjure needs to be executed only once to create one (or more) Essence' models for a problem.
Once the models are generated, they can be used to solve many instances of the same class.

Conjure has been the basis of several research publications. See the following for some examples.

- :cite:`ba536fe7d91a49a8924844c107ffd83e`
- :cite:`1436930e0d004c7fa6adee33e4c98bd6`
- :cite:`c341e68df4c14ab5b77b0693aa1cc90f`
- :cite:`36323c314240451291f8377d5a4f9668`
- :cite:`ae7faa006d3d4ecbb19a14df924e2088`
- :cite:`d868096c9b0a428f83a1d508f98736a5`
- :cite:`17528eb080e74cb6ac61a8387e9ac515`
- :cite:`04b6f9716a204104b26a8a664d101d92`
- :cite:`5d418525265e4c2189449a84ef9db61e`
- :cite:`e74974988ec540bab669443ad10422b3`
- :cite:`4b54ed2e79924712bcf8e6fea3211d72`
- :cite:`ce8510eb25d34db48cd8c3c7682edfdf`
- :cite:`b1d1e65f1ecf4431bc087cff9ef9b9dd`
- :cite:`921a03b374654acdb3cf8b608e1ef86a`
- :cite:`8f516aac022d4bcdb34e5f63976bdd78`
- :cite:`56bd918cab3c4a66b6fae5b71f88b1b6`
- :cite:`413b9d1324cf4826b5ea1a130eb96159`
- :cite:`6eef5285c1a0471ebba55a9179298de8`
- :cite:`8a56ff34e5bc4dada3bcc63d391de55e`
- :cite:`9dd26cea0c54476f8d0418df430909da`
- :cite:`dd9347655a2d45f2bef81ebcb8778daa`
- :cite:`0ef98e79e0bd426eb92227f281f4ee4e`
- :cite:`ec7a3b357c8b4af4ba335c281ea87ba3`
- :cite:`dfc0e4b721844d9d801fb27c264086ff`
- :cite:`066dfdbd563a40c68df2eaae83a342cd`
- :cite:`ed9a3c921f40425b964217636a5af521`
- :cite:`ecb8ca42a8eb4098a685daa84c821da3`
- :cite:`aee46a0c83b14b25b687813677855191`
- :cite:`586c88ee08c1404391bc48c26d66e523`


