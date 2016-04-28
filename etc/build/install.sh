#/bin/bash

# This script will install ghc + cabal + happy + conjure.
# It will only install these tools if they aren't already installed.
#
# It uses a cabal-sanbox, so dependencies are installed locally.
# GHC will be installed under ~/.tools/ghc/${GHC_VERSION}
# cabal-install'ed executables will be under ~/.cabal/bin as usual.
# 
# To clean an installation:
#     rm -rf dist ~/.cabal-sandbox cabal.sandbox.config

# Use environment variables to configure this install script.
# CORES:        number of cores to use while compiling
# GHC_VERSION:  version of ghc to use. the script will download and install ghc under ~/.tools/ghc if not installed.
# OPTIMISATION: "-O0" / "-O1" / "-O2"
# LLVM:         "llvm-on" / "llvm-off"
# BIN_DIR:      where to save the conjure binary


set -o errexit
set -o nounset

export CORES=${CORES:-0}
export GHC_VERSION=${GHC_VERSION:-"7.10.3"}
export GMP_VERSION=${GMP_VERSION:-"NEWGMP"}
export INSTALL_GMP=${INSTALL_GMP:-no}
export OPTIMISATION=${OPTIMISATION:-"-O1"}
export LLVM=${LLVM:-"llvm-off"}
export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}
export BUILD_DOCS=${BUILD_DOCS:-no}
export BUILD_TESTS=${BUILD_TESTS:-no}
export RUN_TESTS=${RUN_TESTS:-no}
export COVERAGE=${COVERAGE:-no}
export PROFILING=${PROFILING:-no}
export DYNAMIC=${DYNAMIC:-no}
export DEVELOPMENT_MODE=${DEVELOPMENT_MODE:-no}


# cabal-install-1.22.0.0 doesn't support the option --disable-executable-profiling
CABAL_VERSION="1.22.6.0"
HAPPY_VERSION="1.19.5"
HSCOLOUR_VERSION="1.20.3"

CABAL_VERSION_CHECK="1.22.6"
HAPPY_VERSION_CHECK="1.19"
HSCOLOUR_VERSION_CHECK="1.20"




OS=$(uname)

if [ "$OS" == "Darwin" ]; then
    PLATFORM="macos"
elif [ "$OS" == "Linux" ]; then
    PLATFORM="linux"
else
    echo "Cannot determine your OS, uname reports: ${OS}"
    exit 1
fi


AVAILABLE_CORES=$( (grep -c ^processor /proc/cpuinfo 2> /dev/null) || (sysctl hw.logicalcpu | awk '{print $2}' 2> /dev/null) || echo 0 )

if [ $CORES -le 0 ]; then
    echo "CORES is set to 0. Will try to use all cores on the machine."
    if [ $AVAILABLE_CORES -le 0 ]; then
        echo "Cannot tell how many cores the machine has. Will use only 1."
        USE_CORES=1
    else
        echo "This machine seems to have $AVAILABLE_CORES cores. Will use all."
        USE_CORES=$AVAILABLE_CORES
    fi
else
    USE_CORES=$CORES
    echo "Using ${USE_CORES} cores."
fi

echo "CORES           : ${CORES}"
echo "AVAILABLE_CORES : ${AVAILABLE_CORES}"
echo "USE_CORES       : ${USE_CORES}"
echo "GMP_VERSION     : ${GMP_VERSION}"
echo "INSTALL_GMP     : ${INSTALL_GMP}"
echo "GHC_VERSION     : ${GHC_VERSION}"
echo "OPTIMISATION    : ${OPTIMISATION}"
echo "LLVM            : ${LLVM}"
echo "CABAL_VERSION   : ${CABAL_VERSION}"
echo "BIN_DIR         : ${BIN_DIR}"
echo "BUILD_DOCS      : ${BUILD_DOCS}"
echo "BUILD_TESTS     : ${BUILD_TESTS}"
echo "RUN_TESTS       : ${RUN_TESTS}"
echo "COVERAGE        : ${COVERAGE}"
echo "PROFILING       : ${PROFILING}"
echo "DYNAMIC         : ${DYNAMIC}"
echo "DEVELOPMENT_MODE: ${DEVELOPMENT_MODE}"


export PATH="${HOME}/.tools/ghc/${GHC_VERSION}/bin":$PATH
export PATH="${HOME}/.cabal/bin":$PATH
export PATH="${BIN_DIR}":$PATH

# installing gmp
if [ $INSTALL_GMP = "yes" ]; then
    mkdir tmp
    pushd tmp
    wget --no-check-certificate -c https://gmplib.org/download/gmp/gmp-6.1.0.tar.bz2
    tar jxf gmp-6.1.0.tar.bz2
    pushd gmp-6.1.0
    ./configure --prefix=$HOME/.tools/libs/gmp-6.1.0
    make
    make check
    make install
    popd
    popd
    rm -rf tmp
fi

# in case we are using a local gmp installation
if [ -d "${HOME}/.tools/libs/gmp-6.1.0/lib" ]; then
    export LIBRARY_PATH=${LIBRARY_PATH:-""}
    export LIBRARY_PATH="${HOME}/.tools/libs/gmp-6.1.0/lib":$LIBRARY_PATH
fi

# installing ghc
if [ "$(ghc --version | grep $GHC_VERSION)" ]; then
    echo "GHC version ${GHC_VERSION} found."
    which ghc
    ghc --version
else
    echo "GHC version ${GHC_VERSION} not found. Installing."
    if [ "$(grep ${GHC_VERSION} ${PLATFORM} ${GMP_VERSION} etc/build/ghc_urls.txt)" ]; then
        GHC_TARBALL=$(grep "${GHC_VERSION} ${PLATFORM} ${GMP_VERSION}" etc/build/ghc_urls.txt | cut -d ' ' -f 4)
        URL="http://www.haskell.org/ghc/dist/${GHC_VERSION}/${GHC_TARBALL}"
        wget --no-check-certificate -c "${URL}"
        tar xvjf "${GHC_TARBALL}"
        pushd "ghc-${GHC_VERSION}"
        mkdir -p "${HOME}/.tools/ghc"
        ./configure --prefix="${HOME}/.tools/ghc/${GHC_VERSION}"
        make install
        popd
        rm -rf "${GHC_TARBALL}" "ghc-${GHC_VERSION}"
    else
        echo "Cannot download GHC version ${GHC_VERSION} for platform ${PLATFORM}."
        exit 1
    fi
fi

# installing cabal-install
if [ "$(cabal --version | head -n 1 | grep ${CABAL_VERSION_CHECK})" ]; then
    echo "cabal-install version ${CABAL_VERSION_CHECK} found."
else
    echo "cabal-install version ${CABAL_VERSION_CHECK} not found. Installing version ${CABAL_VERSION}."
    wget --no-check-certificate -c "http://hackage.haskell.org/packages/archive/cabal-install/${CABAL_VERSION}/cabal-install-${CABAL_VERSION}.tar.gz"
    tar -zxvf "cabal-install-${CABAL_VERSION}.tar.gz"
    pushd "cabal-install-${CABAL_VERSION}"
    bash bootstrap.sh --user --no-doc
    popd
    rm -rf "cabal-install-${CABAL_VERSION}.tar.gz" "cabal-install-${CABAL_VERSION}"
fi

if [ $DEVELOPMENT_MODE = "yes" ]; then
    echo "Skipping: cabal update"
else
    cabal update
fi

# installing happy
if [ "$(happy --version | head -n 1 | grep ${HAPPY_VERSION_CHECK})" ]; then
    echo "happy version ${HAPPY_VERSION_CHECK} found."
else
    echo "happy version ${HAPPY_VERSION_CHECK} not found. Installing version ${HAPPY_VERSION}."
    rm -rf dist/tools
    mkdir -p dist/tools
    pushd dist/tools
    cabal install "happy-${HAPPY_VERSION}" -O2 \
        --force-reinstalls \
        --disable-documentation \
        --disable-library-profiling \
        --disable-executable-profiling
    popd
    rm -rf dist/tools
fi

# installing hscolour
if [ $BUILD_DOCS = "yes" ]; then
    if [ "$(hscolour --version | head -n 1 | grep ${HSCOLOUR_VERSION_CHECK})" ]; then
        echo "hscolour version ${HSCOLOUR_VERSION_CHECK} found."
    else
        echo "hscolour version ${HSCOLOUR_VERSION_CHECK} not found. Installing version ${HSCOLOUR_VERSION}."
        rm -rf dist/tools
        mkdir -p dist/tools
        pushd dist/tools
        cabal install "hscolour-${HSCOLOUR_VERSION}" -O2 \
            --reinstall \
            --force-reinstalls \
            --disable-documentation \
            --disable-library-profiling \
            --disable-executable-profiling
        popd
        rm -rf dist/tools
    fi
fi

ghc   --version
cabal --version
happy --version

make preinstall

if [ $LLVM = "llvm-on" ]; then
    LLVM='--ghc-options="-fllvm"'
else
    LLVM=""
fi

if [ $BUILD_DOCS = "yes" ]; then
    DOCS="--enable-documentation"
else
    DOCS="--disable-documentation"
fi

if [ $BUILD_TESTS = "yes" ]; then
    TESTS="--enable-tests"
else
    TESTS=""
fi


# init sandbox if it doesn't exist

if [ -f cabal.sandbox.config ]; then
    echo "Reusing existing cabal sandbox."
else
    echo "Initialising cabal sandbox."
    cabal sandbox init
fi

# use: hpc markup conjure.tix --destdir=.hpc to generate the html report
# (or hpc markup conjure-testing.tix --destdir=whatevs)
if [ $COVERAGE = "yes" ]; then
    HPC="--ghc-options \"-fhpc\""
else
    HPC=""
fi

if [ ${PROFILING} = "yes" ]; then
    PROFILING="--enable-library-profiling --enable-executable-profiling"
else
    PROFILING="--disable-library-profiling --disable-executable-profiling"
fi

if [ ${DYNAMIC} = "yes" ]; then
    DYNAMIC="--ghc-options \"-dynamic\""
else
    DYNAMIC=""
fi

# install conjure, finally

cabal install                                                           \
    --only-dependencies                                                 \
    --force-reinstalls                                                  \
    --enable-split-objs                                                 \
    ${DYNAMIC} ${PROFILING} ${TESTS} ${DOCS} ${LLVM} ${OPTIMISATION}    \
    --bindir="${BIN_DIR}" -j"${USE_CORES}"

cabal configure                                                         \
    --enable-split-objs                                                 \
    ${DYNAMIC} ${PROFILING} ${HPC} ${TESTS} ${LLVM} ${OPTIMISATION}     \
    --bindir="${BIN_DIR}"

cabal build -j"${USE_CORES}"

if [ $BUILD_DOCS == "yes" ]; then
    cabal haddock --hyperlink-source
fi

cabal copy                                  # install in ${BIN_DIR}

if [ $RUN_TESTS = "yes" ]; then
    time dist/build/conjure-testing/conjure-testing +RTS -s
fi
