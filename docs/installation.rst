
.. _installation:

Installation
============

Conjure can be installed either by downloading a binary distribution, or by compiling it from source code.

Downloading a binary
--------------------

Conjure is available as an executable binary for Linux and MacOS.
If it is available for your platform, you can just `download it <https://www.github.com/conjure-cp/conjure/releases/latest>`_ and run it.
It may be useful to save the binary under a directory that is in your search PATH, so you do not have to type the full path to the Conjure executable to run it.

For Windows, please use the Linux binaries with the
`Windows Subsystem for Linux <https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux>`_.


Compiling from source
---------------------

In order to compile Conjure on your computer, please download the source code from `GitHub <https://github.com/conjure-cp/conjure>`_.

.. code-block:: bash

    git clone git@github.com:conjure-cp/conjure.git
    cd conjure
    BIN_DIR=/somewhere/in/your/path make install

Conjure is implemented in Haskell, it can be compiled using either `cabal-install <http://wiki.haskell.org/Cabal-Install>`_ or `stack <https://docs.haskellstack.org/en/stable/README/>`_.

It comes with a Makefile which will use Stack by default.
The default target in the Makefile will install Stack using the standard procedures (which involves downloading and running a script).
For more precise control, you might want to consider installing the Haskell tools beforehand instead of using the Makefile.

In addition, a number of supported backend solvers can be compiled using the `make solvers` target.
This target also takes a BIN_DIR environment variable to control the location of the solver executables,
and a PROCESSES environment variable to control how many processes to use when building solvers

.. code-block:: bash

    BIN_DIR=/somewhere/in/your/path PROCESSES=4 make solvers

Installing Savile Row
---------------------

Since Conjure works by generating an Essence' model, Savile Row is a vital tool when using it.
You do not need to download Savile Row separately when you compile Conjure from source.
An up-to-date version of Savile Row is also copied next to the Conjure executable.

A standalone version of Savile Row and user documentation for Savile Row can be downloaded from `its website <http://savilerow.cs.st-andrews.ac.uk>`_.


Docker/Podman
-------------

We release an container image that can be used via Docker or Podman.

- The latest release is always at ghcr.io/conjure-cp/conjure:latest

- The latest commit on main is at ghcr.io/conjure-cp/conjure:main

- You can also use a specific version using the SHA of a particular image: ghcr.io/conjure-cp/conjure@sha256:VERSION

See all available images on `Github <https://github.com/conjure-cp/conjure/pkgs/container/conjure>`_.

Inside the container, you will be able to run conjure, savilerow, and almost all supported solvers.

CPLEX
=====

CPLEX is a commercial mathemathical programming solver that is supported by Conjure. However we cannot provide CPLEX as part of the container image due to its license. Instead, we include instructions here for building another image that contains CPLEX as well.

Obtain a commercial or academic license for CPLEX. Use the instructions on `its website <https://www.ibm.com/products/ilog-cplex-optimization-studio>`_. Notes on `this post <https://community.ibm.com/community/user/ai-datascience/blogs/xavier-nodet1/2020/07/09/cplex-free-for-students>`_ are helpful too.

We assume you are on a Linux system here, though steps for macOS are very similar.

- Download the installer. A file called ``cplex_studio2211.linux_x86_64.bin``.
- Run the installer and follow the instructions.
- You can install CPLEX to its default location, however a user-level install is also possible. Assuming you installed it at: ``/home/USER/cplex-install`` for the remaining instructions
- Create a file called ``/home/USER/cplex-install/Containerfile`` with the following contents

.. code-block:: bash

    FROM ghcr.io/conjure-cp/conjure@sha256:VERSION
    COPY cplex /root/.local/
    ENV CPLEX_PATH /root/.local/cplex/bin/x86-64_linux/libcplex2211.so

- In the ``/home/USER/cplex-install`` directory execute: ``podman build -t conjure-cplex .``

- ``podman images`` should now list ``localhost/conjure-cplex`` as well as a bunch of other images.

- You can replace podman with docker in the last 2 commands to use docker instead.












