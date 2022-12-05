
# We use multi-stage builds to end up with a small final image.
# See https://docs.docker.com/build/building/multi-stage
# We have 2 stages, one for building and one just to copy the binaries we want to keep.

################################################################################
# First stage: Building

# Setting up
FROM ubuntu:latest AS builder
WORKDIR /conjure/
COPY . .

# All binaries will end up in /root/.local/bin
RUN mkdir -p /root/.local/bin
ENV PATH /root/.local/bin:$PATH

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

# Building solvers. We do this first to facilitate better caching. Also we don't use `make solvers` here for the same reason.
RUN PROCESSES=2 etc/build/install-bc_minisat_all.sh
RUN PROCESSES=2 etc/build/install-boolector.sh
RUN PROCESSES=2 etc/build/install-cadical.sh
RUN PROCESSES=2 etc/build/install-chuffed.sh
RUN PROCESSES=2 etc/build/install-gecode.sh
RUN PROCESSES=2 etc/build/install-glucose.sh
RUN PROCESSES=2 etc/build/install-kissat.sh
RUN PROCESSES=2 etc/build/install-lingeling.sh
RUN PROCESSES=2 etc/build/install-minion.sh
RUN PROCESSES=2 etc/build/install-nbc_minisat_all.sh
RUN PROCESSES=2 etc/build/install-open-wbo.sh
RUN PROCESSES=2 etc/build/install-yices.sh
RUN PROCESSES=2 etc/build/install-z3.sh

# Building Conjure and copying Savile Row
RUN make install

# Make binaries a bit smaller
RUN ls -l /root/.local/bin
RUN du -sh /root/.local/bin
RUN cd /root/.local/bin ; strip conjure bc_minisat_all_release boolector cadical fzn-chuffed fzn-gecode glucose glucose-syrup kissat lingeling nbc_minisat_all_release open-wbo plingeling treengeling yices yices-sat yices-smt yices-smt2 z3
RUN ls -l /root/.local/bin
RUN du -sh /root/.local/bin


################################################################################
# Second stage: Copying the binaries

FROM ubuntu:latest
WORKDIR /conjure
ENV PATH /root/.local/bin:$PATH
RUN mkdir -p /root/.local/bin/lib
COPY --from=builder /root/.local/bin /root/.local/bin

# Testing
CMD echo "find x : set of int(1..3)" > model.essence ; conjure solve model.essence --number-of-solutions=all ; cat model.solutions

