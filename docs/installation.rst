
.. _installation:

Installation
============

Conjure can be installed either by downloading a binary distribution, or by compiling it from source code.

Downloading a binary
--------------------

Conjure is available as an executable binary for Linux, MacOS, and Windows.
If it is available for your platform, you can just `download it <https://www.github.com/conjure-cp/conjure/releases/latest>`_ and run it.
It may be useful to save the binary under a directory that is in your search PATH, so you do not have to type the full path to the Conjure executable to run it.


Compiling from source
---------------------

In order to compile Conjure on your computer, please download the source code from `GitHub <https://github.com/conjure-cp/conjure>`_.
Conjure is implemented in Haskell, it can be compiled using the standard `cabal-install <http://wiki.haskell.org/Cabal-Install>`_ tool.
It comes with a Makefile which sets things up correctly before running cabal-install.

.. code-block:: bash

    git clone git@github.com:conjure-cp/conjure.git
    cd conjure
    make

It is known to work with
`GHC-7.8.4 <http://www.haskell.org/ghc/download_ghc_7_8_4>`_,
`GHC-7.10.3 <http://www.haskell.org/ghc/download_ghc_7_10_3>`_, and
`GHC-8.0.1 <http://www.haskell.org/ghc/download_ghc_8.0.1>`_.


Installing Savile Row
---------------------

Since Conjure works by generating an Essence' model, Savile Row is a vital tool when using it.
Savile Row can be downloaded from `its website <http://savilerow.cs.st-andrews.ac.uk>`_.

