
# We use multi-stage builds to end up with a small final image.
# See https://docs.docker.com/build/building/multi-stage
# We have 2 stages, one for building and one just to copy the binaries we want to keep.

################################################################################
# First stage: Building

# Setting up
FROM ubuntu:24.04 AS builder
ENV DEBIAN_FRONTEND=noninteractive
WORKDIR /conjure

# All binaries will end up in /opt/conjure/bin
ENV BIN_DIR=/opt/conjure/bin
ENV LIB_DIR=/opt/conjure/lib
RUN mkdir -p $BIN_DIR
RUN mkdir -p $LIB_DIR
ENV PATH=$BIN_DIR:$PATH
ENV LD_LIBRARY_PATH=$LIB_DIR:$LD_LIBRARY_PATH
ENV MZN_STDLIB_DIR=/opt/conjure/share/minizinc

# Dependencies
RUN apt-get update
RUN apt-get install -y --no-install-recommends build-essential          # so we can compile stuff
RUN apt-get install -y --no-install-recommends curl ca-certificates     # so we can download stack (and other things)
RUN apt-get install -y --no-install-recommends xz-utils                 # GHC seems to need xz
RUN apt-get install -y --no-install-recommends libgmp-dev               # GHC definitely needs GMP
RUN apt-get install -y --no-install-recommends zlib1g-dev               # needed when building some solvers (for example bc_minisat_all_release)
RUN apt-get install -y --no-install-recommends cmake                    # needed when building some solvers (for example boolector)
RUN apt-get install -y --no-install-recommends git                      # needed when building some solvers (for example boolector)
RUN apt-get install -y --no-install-recommends zip unzip                # needed when building some solvers (for example glucose)
RUN apt-get install -y --no-install-recommends autoconf                 # needed when building some solvers (for example yices)
RUN apt-get install -y --no-install-recommends gperf                    # needed when building some solvers (for example yices)
RUN apt-get install -y --no-install-recommends python3                  # needed when building some solvers (for example z3)
RUN apt-get install -y --no-install-recommends default-jre-headless     # savilerow
RUN apt-get install -y --no-install-recommends libnuma-dev              # runsolver

# Only copying the install*.sh scripts
RUN mkdir -p etc
COPY etc/build etc/build

# Building solvers. We do this first to facilitate better caching. Also we don't use `make solvers` here for the same reason.
RUN PROCESSES=2 etc/build/install-minisat_all.sh
RUN PROCESSES=2 etc/build/install-boolector.sh
RUN PROCESSES=2 etc/build/install-cadical.sh
RUN PROCESSES=2 etc/build/install-chuffed.sh
RUN PROCESSES=2 etc/build/install-gecode.sh
RUN PROCESSES=2 etc/build/install-glucose.sh
RUN PROCESSES=2 etc/build/install-kissat.sh
RUN PROCESSES=2 etc/build/install-lingeling.sh
RUN PROCESSES=2 etc/build/install-minion.sh
RUN PROCESSES=2 etc/build/install-minizinc.sh
RUN PROCESSES=2 etc/build/install-wmaxcdcl.sh
RUN PROCESSES=2 etc/build/install-ortools.sh
RUN PROCESSES=2 etc/build/install-yices.sh
RUN PROCESSES=2 etc/build/install-z3.sh
RUN PROCESSES=2 etc/build/install-runsolver.sh

# An attempt to cache more
COPY Makefile Makefile
COPY etc/hs-deps etc/hs-deps
COPY conjure-cp.cabal conjure-cp.cabal
RUN make installdeps

# Copy the rest
COPY etc etc
COPY src src
COPY LICENSE LICENSE
RUN make install

# List the binaries
RUN ls -l /opt/conjure/bin
RUN du -sh /opt/conjure

# Copy the allsolvers test case
RUN mkdir -p tests
COPY tests/allsolvers tests/allsolvers

# a test to see if all solvers work as expected
RUN tests/allsolvers/test.sh


################################################################################
# Second stage: Copying the binaries

FROM ubuntu:24.04
WORKDIR /conjure
ENV BIN_DIR=/opt/conjure/bin
ENV LIB_DIR=/opt/conjure/lib
ENV PATH=$BIN_DIR:$PATH
ENV LD_LIBRARY_PATH=$LIB_DIR:$LD_LIBRARY_PATH
ENV MZN_STDLIB_DIR=/opt/conjure/share/minizinc
RUN apt-get update && apt-get install -y --no-install-recommends build-essential          # so we can compile stuff
RUN apt-get update && apt-get install -y --no-install-recommends default-jre-headless     # savilerow
RUN apt-get update && apt-get install -y --no-install-recommends libnuma-dev              # runsolver
RUN mkdir -p /opt/conjure
COPY --from=builder /opt/conjure /opt/conjure
