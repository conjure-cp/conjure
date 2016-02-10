
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


Glossary
--------

Here is a list of some of the terms we use in this document.

.. todo::

    We should probably expalin what these are as well, instead of just listing. Later.

* Constraint Programming
* Constraint Modelling
* CP Model
* Problem specification
* Parameter
* Solution
