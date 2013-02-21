#/bin/bash

# This script will install ghc + cabal + happy + conjure. 
# It will only install these tools if they aren't already installed.
#
# Everything is installed locally. Except ~/.ghc and ~/.cabal and these
# two are used by ghc and cabal for storing some metadata and data about
# installed packages.
#
# To clean an installation:
#     rm dist ~/.ghc ~/.cabal


set -o errexit

WD="$(pwd)"
mkdir -p dist/tools
cd dist/tools

export PATH="$WD/dist/tools/ghc-7.6.2-build/bin":$PATH
export PATH="~/cabal/bin":$PATH


# installing ghc
if [ "$(ghc --version)" != "The Glorious Glasgow Haskell Compilation System, version 7.6.2" ]; then
    echo "Installing GHC"
    wget -c http://www.haskell.org/ghc/dist/7.6.2/ghc-7.6.2-x86_64-apple-darwin.tar.bz2
    tar xvjf ghc-7.6.2-x86_64-apple-darwin.tar.bz2
    cd ghc-7.6.2
    ./configure --prefix="$WD/dist/tools/ghc-7.6.2-build"
    make install
    cd ..
    rm -rf ghc-7.6.2-x86_64-apple-darwin.tar.bz2 ghc-7.6.2
fi


# installing cabal-install
if [ "$(cabal --version | head -n 1)" != "cabal-install version 1.16.0.2" ]; then
    echo "Installing cabal-install"
    wget -c http://hackage.haskell.org/packages/archive/cabal-install/1.16.0.2/cabal-install-1.16.0.2.tar.gz
    tar -zxvf cabal-install-1.16.0.2.tar.gz
    cd cabal-install-1.16.0.2
    bash bootstrap.sh
    cd ..
    rm -rf cabal-install-1.16.0.2.tar.gz cabal-install-1.16.0.2
    cabal update
fi

cd "$WD"


# installing happy
HAS_HAPPY="$(which happy 2> /dev/null > /dev/null ; echo $?)" 
if [ $HAS_HAPPY != 0 ] ; then
    cabal install happy -O2 \
        --force-reinstalls \
        --disable-documentation \
        --disable-library-profiling \
        --disable-executable-profiling
fi

ghc --version
cabal --version
happy --version

# install conjure, finally
cabal update
cabal install -O2 \
    --force-reinstalls \
    --disable-library-profiling \
    --disable-executable-profiling \
    --ghc-options="-funbox-strict-fields" \
    --ghc-options="-fexpose-all-unfoldings"


