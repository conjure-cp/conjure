#/bin/bash

# Use environment variables to configure this install script.
# JOBS:         number of parallel jobs to use while compiling
# GHC_VERSION:  version of ghc to use. the script will download and install ghc under ~/.tools/ghc if not installed.
# OPTIMISATION: "-O0" / "-O1" / "-O2"
# LLVM:         "llvm-on" / "llvm-off"
# BIN_DIR:      where to save the conjure binary

set -o errexit
set -o nounset

OS=$(uname)

if [ "$OS" == "Darwin" ]; then
    PLATFORM="apple-darwin"
elif [ "$OS" == "Linux" ]; then
    PLATFORM="unknown-linux"
else
    echo "Cannot determine your OS, uname reports: ${OS}"
    exit 1
fi

JOBS=${JOBS:-0}
BIN_DIR=${BIN_DIR:-"dist/bin"}

if [ $JOBS -le 0 ]; then
    echo "You didn't set JOBS to anything. Will try to use all CORES this machine has."
    CORES=$( (grep -c ^processor /proc/cpuinfo 2> /dev/null) || (sysctl hw.logicalcpu | awk '{print $2}' 2> /dev/null) || 0 )
    if [ $CORES -eq 0 ]; then
        echo "Cannot tell how many cores the machine has. Will use only 1."
    else
        echo "This machine seems to have $CORES cores. Will use all."
        JOBS=$CORES
    fi
else
    CORES="${JOBS}"
    echo "Using ${CORES} cores."
fi

CABAL_VERSION="1.18.0.2"


echo "JOBS          : ${JOBS}"
echo "CORES         : ${CORES}"
echo "GHC_VERSION   : ${GHC_VERSION}"
echo "OPTIMISATION  : ${OPTIMISATION}"
echo "LLVM          : ${LLVM}"
echo "CABAL_VERSION : ${CABAL_VERSION}"
echo "BIN_DIR       : ${BIN_DIR}"


export PATH="${HOME}/.tools/ghc/${GHC_VERSION}/bin":$PATH
export PATH="${HOME}/.cabal/bin":$PATH


# installing ghc
if [ "$(ghc --version | grep $GHC_VERSION)" ]; then
    echo "GHC version ${GHC_VERSION} found."
    which ghc
    ghc --version
else
    echo "GHC version ${GHC_VERSION} not found. Installing."
    wget -c "http://www.haskell.org/ghc/dist/${GHC_VERSION}/ghc-${GHC_VERSION}-x86_64-${PLATFORM}.tar.bz2"
    tar xvjf "ghc-${GHC_VERSION}-x86_64-${PLATFORM}.tar.bz2"
    pushd "ghc-${GHC_VERSION}"
    mkdir -p "${HOME}/.tools/ghc"
    ./configure --prefix="${HOME}/.tools/ghc/${GHC_VERSION}"
    make install
    popd
    rm -rf "ghc-${GHC_VERSION}-x86_64-${PLATFORM}.tar.bz2" "ghc-${GHC_VERSION}"
fi


# installing cabal-install
if [ "$(cabal --version | head -n 1 | grep ${CABAL_VERSION})" ]; then
    echo "cabal install version ${CABAL_VERSION} found."
else
    echo "cabal install version ${GHC_VERSION} not found. Installing."
    wget -c "http://hackage.haskell.org/packages/archive/cabal-install/${CABAL_VERSION}/cabal-install-${CABAL_VERSION}.tar.gz"
    tar -zxvf "cabal-install-${CABAL_VERSION}.tar.gz"
    pushd "cabal-install-${CABAL_VERSION}"
    bash bootstrap.sh
    popd
    rm -rf "cabal-install-${CABAL_VERSION}.tar.gz" "cabal-install-${CABAL_VERSION}"
fi

cabal update

# installing happy
HAS_HAPPY="$(which happy 2> /dev/null > /dev/null ; echo $?)" 
if [ "$HAS_HAPPY" != 0 ] ; then
    echo "Installing happy"
    mkdir -p dist/tools
    pushd dist/tools
    cabal install happy -O2 \
        --force-reinstalls \
        --disable-documentation \
        --disable-library-profiling \
        --disable-executable-profiling
    popd
fi

ghc   --version
cabal --version
happy --version

VERSION=$(hg id -i | head -n 1)
echo "Generating src/RepositoryVersion.hs with version ${VERSION}."
echo "module RepositoryVersion where"       >  src/RepositoryVersion.hs
echo "repositoryVersion :: String"          >> src/RepositoryVersion.hs
echo "repositoryVersion = \"${VERSION}\""   >> src/RepositoryVersion.hs


if [ $LLVM = "llvm-on" ]; then
    LLVM='--ghc-options="-fllvm"'
else
    LLVM=""
fi


# init sandbox if it doesn't exist

if [ -f cabal.sandbox.config ]; then
    echo "Reusing existing cabal sandbox."
else
    echo "Initialising cabal sandbox."
    cabal sandbox init
fi


# install conjure, finally

cabal install                                                       \
    --disable-documentation                                         \
    --disable-library-profiling --disable-executable-profiling      \
    --force-reinstalls                                              \
    ${LLVM} ${OPTIMISATION} --bindir="${BIN_DIR}" -j"${JOBS}"       \
    --only-dependencies

cabal install                                                       \
    --disable-documentation                                         \
    --disable-library-profiling --disable-executable-profiling      \
    --force-reinstalls                                              \
    ${LLVM} ${OPTIMISATION} --bindir="${BIN_DIR}" -j1

