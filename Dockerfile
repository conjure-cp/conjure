FROM ubuntu:latest
WORKDIR /conjure/
COPY . .
RUN apt-get update
RUN apt-get install -y --no-install-recommends make curl ca-certificates xz
ENV PATH /root/.local/bin:$PATH
RUN make
CMD conjure --version


# # Dockerfile for Conjure
# #
# # Conjure requires Haskell, JRE, CMake, C++, etc.
# # This is based on a merge of the two main parents
# # https://github.com/freebroccolo/docker-haskell/blob/master/8.4/Dockerfile
# # https://github.com/docker-library/openjdk/blob/master/11/jre/slim/Dockerfile
# # with the other components on top of a slim Debian base image.

# FROM debian:stable-slim

# ARG BRANCH=v2.4.0

# ENV LANG        C.UTF-8
# ENV WORK        /conjure

# # Stop tzdata prompt
# ENV DEBIAN_FRONTEND=noninteractive

# # create a JAVA_HOME that's cross-architecture-safe
# ENV JAVA_HOME /docker-java-home
# RUN ln -svT "/usr/lib/jvm/java-11-openjdk-$(dpkg --print-architecture)" $JAVA_HOME
# ENV JAVA_VERSION 11

# RUN set -ex; \
# # slim variants have no man pages (causes "update-alternatives" to fail)
#     if [ ! -d /usr/share/man/man1 ]; then \
#         mkdir -p /usr/share/man/man1; \
#     fi; \
#     apt-get update; \
#     apt-get install -y --no-install-recommends \
#      openjdk-11-jre-headless; \
# # update-alternatives so future installs of OpenJDK don't change /usr/bin/java
#     update-alternatives --get-selections | awk -v home="$(readlink -f "$JAVA_HOME")" 'index($3, home) == 1 { $2 = "manual"; print | "update-alternatives --set-selections" }'; \
# # ... and verify it worked for one of the alternatives
#     update-alternatives --query java | grep -q 'Status: manual'

# RUN apt-get update \
# &&  apt-get install -y --no-install-recommends \
#      ca-certificates \
#      dirmngr \
#      git \
#      gnupg \
#  && apt-get install -y --no-install-recommends \
#      bison \
#      cmake \
#      flex \
#      g++ \
#      libsqlite3-dev \
#      libtinfo-dev \
#      make \
#      netbase \
#      wget \
#      zlib1g-dev \
#      xz-utils \
#      libgmp-dev

# WORKDIR $WORK

# # default Haskell stack build usually fails on Docker, instead
# # install a known-to-work binary build, see:
# # https://github.com/commercialhaskell/stack/issues/3510#issuecomment-386266579
# RUN apt-get install -y --no-install-recommends \
#      curl \
#  && curl -fSL https://github.com/commercialhaskell/stack/releases/download/v1.7.1/stack-1.7.1-linux-x86_64.tar.gz -o stack.tar.gz \
#  && curl -fSL https://github.com/commercialhaskell/stack/releases/download/v1.7.1/stack-1.7.1-linux-x86_64.tar.gz.asc -o stack.tar.gz.asc \
#  && apt-get purge -y --auto-remove curl \
#  && tar -xf stack.tar.gz -C /usr/local/bin --strip-components=1 \
#  && /usr/local/bin/stack config set system-ghc --global true \
#  && /usr/local/bin/stack config set install-ghc --global false \
#  && rm -f stack.tar.gz.asc stack.tar.gz

# ENV PATH        /root/.cabal/bin:/root/.local/bin:$PATH
# ENV BIN_DIR     /root/.local/bin

# RUN git clone https://github.com/conjure-cp/conjure.git
# WORKDIR $WORK/conjure
# RUN git pull
# RUN git checkout $BRANCH

# RUN make
# # RUN PROCESSES=4 make solvers
# RUN etc/build/install-minion.sh
# RUN etc/build/install-lingeling.sh


# RUN apt-get purge -y --auto-remove \
#      bison \
# # JRE depends on this, don't remove:
# #     ca-certificates \
#      cmake \
#      dirmngr \
#      flex \
#      g++ \
#      git \
#      gnupg \
#      libsqlite3-dev \
#      libtinfo-dev \
#      make \
#      netbase \
#      wget \
#      zlib1g-dev \
#  && rm -rf /var/lib/apt/lists/*

# WORKDIR $WORK

# RUN rm -f pubring.kbx* trustdb.gpg /root/.wget-hsts \
#  && rm -rf crls.d private-keys-v1.d \
#  && rm -rf conjure \
# # remove stack
#  && rm -rf /root/.stack \
#  && rm -rf /usr/local/bin/*

# ENV PATH        /root/.local/bin:$PATH

# RUN rm -f pubring.kbx* trustdb.gpg /root/.wget-hsts \
#  && rm -rf crls.d private-keys-v1.d \
#  && rm -rf conjure \
# # remove stack
#  && rm -rf /root/.stack \
#  && rm -rf /usr/local/bin/*

# ENV PATH        /root/.local/bin:$PATH

# # do a test-run of the pipeline, unless command is specified
# COPY sudoku.essence sudoku.param $WORK/
# CMD conjure solve -ac --solutions-in-one-file --number-of-solutions=all --solver=minion --limit-time=90 sudoku.essence sudoku.param \
#  && mv conjure-output/model000001-sudoku.solutions /tmp/result.txt \
#  && cat /tmp/result.txt \
#  && rm /tmp/result.txt \
#  && rm -rf conjure-output
