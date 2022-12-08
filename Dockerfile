
# We use multi-stage builds to end up with a small final image.
# See https://docs.docker.com/build/building/multi-stage
# We have 2 stages, one for building and one just to copy the binaries we want to keep.

################################################################################
# First stage: Building

# Setting up
FROM alpine:3.17 AS builder
WORKDIR /conjure/

# All binaries will end up in /root/.local/bin
RUN mkdir -p /root/.local/bin
ENV PATH /root/.local/bin:$PATH

# Dependencies
RUN apk add --no-cache build-base               # some basics
RUN apk add --no-cache bash                     # might as well have bash
RUN apk add --no-cache gcompat                  # turns out build-base does not contain libc, https://wiki.alpinelinux.org/wiki/Running_glibc_programs
RUN apk add --no-cache curl ca-certificates     # so we can download stack (and other things)
RUN apk add --no-cache xz                       # GHC seems to need xz
RUN apk add --no-cache gmp-dev                  # GHC definitely needs GMP
RUN apk add --no-cache zlib-dev                 # needed when building some solvers (for example bc_minisat_all_release)
RUN apk add --no-cache cmake                    # needed when building some solvers (for example boolector)
RUN apk add --no-cache git                      # needed when building some solvers (for example boolector)
RUN apk add --no-cache zip unzip                # needed when building some solvers (for example glucose)
RUN apk add --no-cache autoconf                 # needed when building some solvers (for example yices)
RUN apk add --no-cache gperf                    # needed when building some solvers (for example yices)
RUN apk add --no-cache python3                  # needed when building some solvers (for example z3)

# Only copying the install*.sh scripts
RUN mkdir -p etc
COPY etc/build etc/build

# Building solvers. We do this first to facilitate better caching. Also we don't use `make solvers` here for the same reason.
RUN bash etc/build/install-bc_minisat_all.sh
RUN bash etc/build/install-boolector.sh
RUN bash etc/build/install-cadical.sh
RUN bash etc/build/install-chuffed.sh
RUN bash etc/build/install-gecode.sh
RUN bash etc/build/install-glucose.sh
RUN bash etc/build/install-kissat.sh
RUN bash etc/build/install-lingeling.sh
RUN bash etc/build/install-minion.sh
RUN bash etc/build/install-nbc_minisat_all.sh
RUN bash etc/build/install-open-wbo.sh
RUN bash etc/build/install-yices.sh
RUN bash etc/build/install-z3.sh

# Copy everything
COPY . .

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

FROM alpine:latest
WORKDIR /conjure
ENV PATH /root/.local/bin:$PATH
RUN mkdir -p /root/.local/bin/lib
COPY --from=builder /root/.local/bin /root/.local/bin

# Testing
CMD echo "find x : set of int(1..3)" > model.essence ; conjure solve model.essence --number-of-solutions=all ; cat model.solutions

