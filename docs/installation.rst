
.. _installation:

Installation
============

Conjure can be installed either by downloading a binary distribution, or by compiling it from source code.

It can also be used directly via a container that we release on GitHub. **This is the suggested way of using Conjure**, see :ref:`docker-podman-apptainer` section below. Make sure you read the entire section including :ref:`shell-wrapper`.

Downloading a binary
--------------------

Conjure is available as an executable binary for Linux and MacOS.
If it is available for your platform, you can just `download it <https://www.github.com/conjure-cp/conjure/releases/latest>`_ and run it.
It may be useful to save the binary under a directory that is in your search PATH, so you do not have to type the full path to the Conjure executable to run it.

You can add a directory into your PATH by executing the following commands in the terminal (assuming you extract the zip directory and `mv`` it to `~/work/conjure` - feel free to change the location to fit your workflow better)

.. code-block:: bash

    export PATH=~/work/conjure:$PATH
    export LD_LIBRARY_PATH=~/work/conjure/lib:$LD_LIBRARY_PATH
    export MZN_STDLIB_DIR=~/work/conjure/share/minizinc/


For MacOS, you will have to remove the quarantine attribute from all files and directories inside the downloaded directory. To do this, navigate to the directory and run the following command.

.. code-block:: bash

    xattr -dr com.apple.quarantine .


You will also need to install Java for Savile Row. In macOS, we recommend using Homebrew and Amazon Corretto. Install Homebrew if you don't have it, then run the following command.

.. code-block:: bash

    brew install --cask corretto

If you are using a recent Mac, you *might* need to run the following commmand to allow Intel-based applications to tun on your computer.

.. code-block:: bash

    /usr/sbin/softwareupdate --install-rosetta --agree-to-license


For Windows, please use the Linux binaries with the
`Windows Subsystem for Linux <https://en.wikipedia.org/wiki/Windows_Subsystem_for_Linux>`_.

To Install into wsl2 on Windows then use the following commands 

.. code-block:: bash

     cd 
     wget https://github.com/conjure-cp/conjure/releases/download/v2.6.0/conjure-v2.6.0-linux-with-solvers.zip
     unzip conjure-v2.6.0-linux-with-solvers.zip
     echo 'export PATH="$HOME/conjure-v2.6.0-linux-with-solvers:$PATH"' >> ~/.zshrc
     echo 'export LD_LIBRARY_PATH="$HOME/conjure-v2.6.0-linux-with-solvers/lib:$LD_LIBRARY_PATH"' >> ~/.zshrc
     
Then restart your shell!

Compiling from source
---------------------

In order to compile Conjure on your computer, please download the source code from `GitHub <https://github.com/conjure-cp/conjure>`_.

.. code-block:: bash

    git clone https://github.com/conjure-cp/conjure.git
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


.. _docker-podman-apptainer:

Docker / Podman / Apptainer
---------------------------

Conjure is distributed as a container image via the GitHub Container Registry.
Using a container allows Conjure (together with Savile Row and supported
solvers) to be run without a local installation, and makes it easy to pin a
specific version for reproducibility.

The container image includes:

- ``conjure``
- ``savilerow``
- almost all solvers supported by Conjure

Available images
^^^^^^^^^^^^^^^^

- Latest release:
  
  ``ghcr.io/conjure-cp/conjure:latest``

- Latest commit on ``main`` (i.e. the development version):

  ``ghcr.io/conjure-cp/conjure:main``

- A specific immutable image (recommended for reproducibility):

  ``ghcr.io/conjure-cp/conjure@sha256:VERSION``

A full list of available images and tags is available at:
https://github.com/conjure-cp/conjure/pkgs/container/conjure

Docker and Podman usage
^^^^^^^^^^^^^^^^^^^^^^

The examples below assume that:

- Docker or Podman is installed and running
- Your current directory contains:
  
  - an Essence model (e.g. ``test.essence``)
  - a parameter file (e.g. ``sample.param``)

All commands are run from that directory.

Basic example
"""""""""""""

The following command is equivalent to running::

  conjure solve test.essence sample.param

Using Docker:

.. code-block:: bash

   docker run --rm \
     -v "$PWD:/work" \
     -w /work \
     ghcr.io/conjure-cp/conjure:v2.6.0 \
     conjure solve test.essence sample.param

Using Podman:

.. code-block:: bash

   podman run --rm \
     -v "$PWD:/work" \
     -w /work \
     ghcr.io/conjure-cp/conjure:v2.6.0 \
     conjure solve test.essence sample.param

Explanation
"""""""""""

- ``-v "$PWD:/work"``
  
  Mounts the current host directory into the container at ``/work``.

- ``-w /work``
  
  Sets the container working directory so Conjure can find input files and write
  outputs.

- ``--rm``
  
  Removes the container after execution. Output files remain on the host.

Output files
""""""""""""

By default, Conjure writes outputs to the working directory. After running the
command, you should see:

- a ``conjure-output/`` directory
- one or more solution files (depending on the problem and solver)

All outputs are written directly to the host filesystem.

Apptainer usage
^^^^^^^^^^^^^^^

The same container image can be used with Apptainer (formerly Singularity),
which is common on HPC systems.

.. code-block:: bash

   apptainer exec docker://ghcr.io/conjure-cp/conjure:v2.6.0 \
     conjure solve test.essence sample.param

In this case, Apptainer automatically binds the current directory, so input and
output files are available without additional flags.


Version pinning
^^^^^^^^^^^^^^^

For fully reproducible experiments, use either a fixed version tag (e.g.
``v2.6.0``) or a SHA-based image reference. This guarantees that the same Conjure,
Savile Row, and solver versions are used across machines and over time.

.. _shell-wrapper:

Shell wrapper (optional, but probably a good idea!)
^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^^

If you frequently run Conjure via a container, you may find it convenient to
define a shell wrapper so that ``conjure`` behaves like a locally installed
command.

The examples below assume Bash or Zsh.

Docker
""""""

Add the following function to your ``.bashrc`` or ``.zshrc``:

.. code-block:: bash

   conjure() {
     docker run --rm \
       -v "$PWD:/work" \
       -w /work \
       ghcr.io/conjure-cp/conjure:v2.6.0 \
       conjure "$@"
   }

Podman
""""""

.. code-block:: bash

   conjure() {
     podman run --rm \
       -v "$PWD:/work" \
       -w /work \
       ghcr.io/conjure-cp/conjure:v2.6.0 \
       conjure "$@"
   }

Apptainer
"""""""""

Apptainer automatically binds the current directory, so no explicit volume
mount is required.

.. code-block:: bash

   conjure() {
     apptainer exec docker://ghcr.io/conjure-cp/conjure:v2.6.0 \
       conjure "$@"
   }

Usage
"""""

After defining one of the wrappers above, you can run:

.. code-block:: bash

   conjure solve test.essence sample.param

The wrapper forwards all arguments to Conjure unchanged.

Notes
"""""

- Only define **one** of the wrappers, corresponding to the container runtime
  you use.
- To change Conjure versions, update the image tag in the wrapper.
- For fully reproducible setups, consider using a SHA-based image reference
  instead of a version tag.


Authentication note (GHCR)
""""""""""""""""""""""""""

If pulling the image fails with an error such as::

  denied: denied

this is usually caused by cached credentials for ``ghcr.io``. Logging out and
retrying typically resolves the issue::

  docker logout ghcr.io

Or the same command for `podman`. Apptainer doesn't use the same credential-based system for accessing GHCR and doesn't have a `logout` command.

Testing your installation
-------------------------

Each release ships a small solver smoke-test bundle called
``conjure-allsolver-test-v2.6.0.zip``. It is the contents of
``tests/allsolvers`` and includes ``run.sh``, ``test.sh``,
``test.essence``, ``testo.essence``, and ``stdout.expected``.

Download the zip from the release page, extract it, and change into the
extracted directory. If you compiled Conjure yourself, downloaded the
precompiled executables, or set up the container shell wrapper above, then
``conjure`` is already on your ``PATH`` and you can run the tests directly.
If you did not set up the wrapper, use the container commands below.

Direct run (compiling from source, precompiled executables, or wrapper alias):

.. code-block:: bash

   bash test.sh

On success you will see ``Pass!`` and a zero exit code. If you want to see the
solver output without the diff check, run ``run.sh`` instead.

Container run (no wrapper alias):

.. code-block:: bash

   docker run --rm \
     -v "$PWD:/work" \
     -w /work \
     ghcr.io/conjure-cp/conjure:v2.6.0 \
     bash test.sh

.. code-block:: bash

   podman run --rm \
     -v "$PWD:/work" \
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












