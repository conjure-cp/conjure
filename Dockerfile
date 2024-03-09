
# We use multi-stage builds to end up with a small final image.
# See https://docs.docker.com/build/building/multi-stage
# We have 2 stages, one for building and one just to copy the binaries we want to keep.

################################################################################
# First stage: Building

# Setting up
FROM ubuntu:23.10 AS builder
ENV DEBIAN_FRONTEND noninteractive
WORKDIR /conjure/

# All binaries will end up in /root/.local/bin
RUN mkdir -p /root/.local/bin
ENV PATH /root/.local/bin:$PATH
ENV LD_LIBRARY_PATH /root/.local/bin/lib:$LD_LIBRARY_PATH
ENV MZN_STDLIB_DIR /root/.local/bin/share/minizinc/
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

# Only copying the install*.sh scripts
RUN mkdir -p etc
COPY etc/build etc/build

# Building solvers. We do this first to facilitate better caching. Also we don't use `make solvers` here for the same reason.
RUN PROCESSES=2 etc/build/install-bc_minisat_all.sh
RUN PROCESSES=2 etc/build/install-boolector.sh
RUN PROCESSES=2 etc/build/install-cadical.sh
RUN PROCESSES=2 etc/build/install-chuffed.sh
# RUN PROCESSES=2 etc/build/install-gecode.sh
RUN PROCESSES=2 etc/build/install-glucose.sh
RUN PROCESSES=2 etc/build/install-kissat.sh
RUN PROCESSES=2 etc/build/install-lingeling.sh
RUN PROCESSES=2 etc/build/install-minion.sh
RUN PROCESSES=2 etc/build/install-minizinc.sh
RUN PROCESSES=2 etc/build/install-nbc_minisat_all.sh
RUN PROCESSES=2 etc/build/install-open-wbo.sh
RUN PROCESSES=2 etc/build/install-ortools.sh
RUN PROCESSES=2 etc/build/install-yices.sh
RUN PROCESSES=2 etc/build/install-z3.sh

# An attempt to cache more
COPY Makefile Makefile
COPY etc/hs-deps etc/hs-deps
RUN make installdeps

# Copy the rest
COPY etc etc
COPY src src
RUN make install

# List the binaries
RUN ls -l /root/.local/bin
RUN du -sh /root/.local/bin

# a test to see if all solvers work as expected
RUN tests/allsolvers/test.sh


################################################################################
# Second stage: Copying the binaries

FROM ubuntu:23.10
WORKDIR /conjure
ENV PATH /root/.local/bin:$PATH
ENV LD_LIBRARY_PATH /root/.local/bin/lib:$LD_LIBRARY_PATH
ENV MZN_STDLIB_DIR /root/.local/bin/share/minizinc/
RUN apt-get update
RUN apt-get install -y --no-install-recommends default-jre-headless     # savilerow
RUN mkdir -p /root/.local/bin/lib
COPY --from=builder /root/.local/bin /root/.local/bin
