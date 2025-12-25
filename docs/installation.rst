.. _installation:

Installation
============

Conjure can be installed either by downloading a binary distribution, or by compiling it from source code.

It can also be used directly via a container that we release on GitHub. **This is the suggested way of using Conjure**, see :ref:`container-quickstart` and :ref:`docker-podman-apptainer` below.

Why containers are recommended
------------------------------

Using Conjure via a container avoids the need to install and manage its dependencies locally, including Savile Row and backend solvers. Containers also make it easy to pin exact versions of Conjure and solvers, ensuring reproducibility across machines and over time. For most users, especially when running experiments or teaching, containers provide the simplest and most robust setup.

.. _container-quickstart:

Quick start using containers
----------------------------

If you have Docker, Podman, or Apptainer working locally, the simplest way to use Conjure is to install a small wrapper script that runs Conjure inside a container. This makes ``conjure`` available as a normal command, both interactively and from scripts.

The examples below install the wrapper in ``~/.local/bin``. **Make sure this directory is on your ``PATH``**.

Docker
^^^^^^

.. code-block:: bash

    # create the directory if it doesn't exist
    mkdir -p ~/.local/bin

    # create the conjure wrapper
    echo 'docker run --rm -v "$PWD:/work" -w /work ghcr.io/conjure-cp/conjure:v2.6.0 conjure "$@"' > ~/.local/bin/conjure

    # make it executable
    chmod +x ~/.local/bin/conjure

Podman
^^^^^^

.. code-block:: bash

    # create the directory if it doesn't exist
    mkdir -p ~/.local/bin

    # create the conjure wrapper
    echo 'podman run --rm -v "$PWD:/work:z" -w /work ghcr.io/conjure-cp/conjure:v2.6.0 conjure "$@"' > ~/.local/bin/conjure

    # make it executable
    chmod +x ~/.local/bin/conjure

Apptainer
^^^^^^^^^

.. code-block:: bash

    # create the directory if it doesn't exist
    mkdir -p ~/.local/bin

    # create the conjure wrapper
    echo 'apptainer exec docker://ghcr.io/conjure-cp/conjure:v2.6.0 conjure "$@"' > ~/.local/bin/conjure

    # make it executable
    chmod +x ~/.local/bin/conjure

The container will be downloaded first time you run the ``conjure`` command defined this way.

After installation, verify that Conjure is available:

.. code-block:: bash

    conjure --version

You can also verify that the installed solvers are all operational:

.. code-block:: bash

    # download a small bundle of files, we will use this to test the installation
    wget https://github.com/conjure-cp/conjure/releases/download/v2.6.0/conjure-allsolver-test-v2.6.0.zip

    # run the test
    unzip conjure-allsolver-test-v2.6.0.zip && bash test.sh

``test.sh`` should produce a bunch of output and include "Pass!" as the last line.

Note that this quick start guide is not a replacement for the complete installation instructions; please keep reading to learn more.

Downloading a binary
--------------------

Conjure is available as an executable binary for Linux and macOS.
If it is available for your platform, you can just `download it <https://www.github.com/conjure-cp/conjure/releases/latest>`_ and run it.

It may be useful to save the binary under a directory that is in your ``PATH``, so you do not have to type the full path to the Conjure executable.

Assuming you extract the zip archive to ``~/work/conjure``:

.. code-block:: bash

    export PATH=~/work/conjure:$PATH
    export LD_LIBRARY_PATH=~/work/conjure/lib:$LD_LIBRARY_PATH
    export MZN_STDLIB_DIR=~/work/conjure/share/minizinc/

For macOS, remove the quarantine attribute from all files and directories inside the downloaded directory:

.. code-block:: bash

    xattr -dr com.apple.quarantine .

You will also need Java for Savile Row. On macOS, we recommend Homebrew and Amazon Corretto:

.. code-block:: bash

    brew install --cask corretto

On recent Macs, you *might* need to allow Intel-based applications to run:

.. code-block:: bash

    /usr/sbin/softwareupdate --install-rosetta --agree-to-license


For Windows, please use the Linux binaries with the
`Windows Subsystem for Linux <https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux>`_.

To install into WSL2:

.. code-block:: bash

    cd
    wget https://github.com/conjure-cp/conjure/releases/download/v2.6.0/conjure-v2.6.0-linux-with-solvers.zip
    unzip conjure-v2.6.0-linux-with-solvers.zip
    echo 'export PATH="$HOME/conjure-v2.6.0-linux-with-solvers:$PATH"' >> ~/.zshrc
    echo 'export LD_LIBRARY_PATH="$HOME/conjure-v2.6.0-linux-with-solvers/lib:$LD_LIBRARY_PATH"' >> ~/.zshrc

Then restart your shell.

Compiling from source
---------------------

To compile Conjure yourself, download the source code from `GitHub <https://github.com/conjure-cp/conjure>`_.

.. code-block:: bash

    git clone https://github.com/conjure-cp/conjure.git
    cd conjure
    BIN_DIR=/somewhere/in/your/path make install

Conjure is implemented in Haskell and can be compiled using either `cabal-install <http://wiki.haskell.org/Cabal-Install>`_ or `stack <https://docs.haskellstack.org/>`_.

It comes with a Makefile which will use Stack by default.
The default target in the Makefile will install Stack using the standard procedures (which involves downloading and running a script).
For more precise control, you might want to consider installing the Haskell tools beforehand instead of using the Makefile.

In addition, a number of supported backend solvers can be compiled using the ``make solvers`` target.
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


.. _docker-podman-apptainer:

Docker / Podman / Apptainer
---------------------------

Conjure is distributed as a container image via the GitHub Container Registry.
Using a container allows Conjure (together with Savile Row and supported solvers) to be run without a local installation, and makes it easy to pin a specific version for reproducibility.

The container image includes:

- ``conjure``
- ``savilerow``
- almost all supported solvers

Available images
^^^^^^^^^^^^^^^^

- Latest release:

  ``ghcr.io/conjure-cp/conjure:latest``

- A specific release:

  ``ghcr.io/conjure-cp/conjure:v2.6.0``

- Latest commit on ``main`` (development version):

  ``ghcr.io/conjure-cp/conjure:main``

- Immutable image (recommended for reproducibility):

  ``ghcr.io/conjure-cp/conjure@sha256:VERSION``

See all available images at: https://github.com/conjure-cp/conjure/pkgs/container/conjure

Docker and Podman usage
^^^^^^^^^^^^^^^^^^^^^^

The examples below assume:

- Docker or Podman is installed and running
- The current directory contains:
  - an Essence model (e.g. ``test.essence``)
  - a parameter file (e.g. ``sample.param``)

Docker:

.. code-block:: bash

    docker run --rm \
      -v "$PWD:/work" \
      -w /work \
      ghcr.io/conjure-cp/conjure:v2.6.0 \
      conjure solve test.essence sample.param

Podman:

.. code-block:: bash

    podman run --rm \
      -v "$PWD:/work:z" \
      -w /work \
      ghcr.io/conjure-cp/conjure:v2.6.0 \
      conjure solve test.essence sample.param

Note that ``:z`` above is for handling SELinux. A side-effect of this is that you will not be able to run conjure this command inside your home directory. If you try to do so, you will get the following error message: "Error: SELinux relabeling of <your home directory> is not allowed".

Apptainer usage
^^^^^^^^^^^^^^^

.. code-block:: bash

    apptainer exec docker://ghcr.io/conjure-cp/conjure:v2.6.0 \
      conjure solve test.essence sample.param

Version pinning
^^^^^^^^^^^^^^^

For fully reproducible experiments, use a fixed version tag or a SHA-based image reference.


Authentication note (GHCR)
^^^^^^^^^^^^^^^^^^^^^^^^^^

If pulling the image fails with an error such as::

  denied: denied

this is usually caused by cached credentials for ``ghcr.io``.
Logging out and retrying typically resolves the issue::

  docker logout ghcr.io

or the equivalent command for Podman.

Apptainer does not use Docker/Podman credential stores and does not provide a
``logout`` command.

Testing your installation
-------------------------

Each release ships a solver smoke-test bundle called ``conjure-allsolver-test-v2.6.0.zip``.

If you installed one of the wrappers above, ``conjure`` is already on your ``PATH`` and you can run the tests directly:

.. code-block:: bash

    bash test.sh

On success you will see ``Pass!`` and a zero exit code.

Container-only execution (no wrapper):

.. code-block:: bash

    docker run --rm \
      -v "$PWD:/work" \
      -w /work \
      ghcr.io/conjure-cp/conjure:v2.6.0 \
      bash test.sh

.. code-block:: bash

    podman run --rm \
      -v "$PWD:/work:z" \
      -w /work \
      ghcr.io/conjure-cp/conjure:v2.6.0 \
      bash test.sh

.. code-block:: bash

    apptainer exec docker://ghcr.io/conjure-cp/conjure:v2.6.0 \
      bash test.sh





CPLEX with Docker/Podman
------------------------

CPLEX is a commercial mathemathical programming solver that is supported by Conjure. However we cannot provide CPLEX as part of the container image due to its license. Instead, we include instructions here for building another image that contains CPLEX as well.

Obtain a commercial or academic license for CPLEX. Use the instructions on `its website <https://www.ibm.com/products/ilog-cplex-optimization-studio>`_. Notes on `this post <https://community.ibm.com/community/user/ai-datascience/blogs/xavier-nodet1/2020/07/09/cplex-free-for-students>`_ are helpful too.

We assume you are on a Linux system here, though steps for macOS are very similar.

- Download the installer. A file called ``cplex_studio2211.linux_x86_64.bin``.
- Run the installer and follow the instructions.
- You can install CPLEX to its default location, however a user-level install is also possible. Assuming you installed it at: ``/home/USER/cplex-install`` for the remaining instructions
- Create a file called ``/home/USER/cplex-install/Dockerfile`` with the following contents

.. code-block:: bash

    FROM ghcr.io/conjure-cp/conjure@sha256:VERSION
    COPY cplex /root/.local/cplex
    ENV CPLEX_PATH /root/.local/cplex/bin/x86-64_linux/libcplex2211.so

- In the ``/home/USER/cplex-install`` directory execute: ``podman build -t conjure-cplex .``

- ``podman images`` should now list ``localhost/conjure-cplex`` as well as a bunch of other images.

- You can replace podman with docker in the last 2 commands to use docker instead.












