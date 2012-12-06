#!/bin/bash

# This scripts assumes the following:
#   it is being run on a Linux/OSX machine.
#   has the correct versions of ghc and cabal installed.
#   has hg installed.
#   has a ~/.profile file for $PATH manipilations and such.
#   can clone bitbucket/ozgurakgun/conjure (public key uploaded to bitbucket)

. ~/.profile
if [ -d ~/repos/conjure ]; then
    cd ~/repos/conjure
    hg pull
    hg update
else
    mkdir -p ~/repos
    cd ~/repos
    hg clone ssh://hg@bitbucket.org/ozgurakgun/conjure
fi
cd ~/repos/conjure
cabal update
cabal install --disable-documentation \
              --enable-library-profiling \
              --disable-executable-profiling \
              --only-dependencies \
              --force-reinstalls \
              -O2
scripts/build/make -O

