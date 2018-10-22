#!/bin/bash

# This script will install ghc + cabal + conjure.
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

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source "${SCRIPT_DIR}"/default_envvars.sh

CABAL_VERSION="2.2.0.0"
CABAL_VERSION_CHECK="2.2.0.0"


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
echo "INSTALL_GHC     : ${INSTALL_GHC}"
echo "INSTALL_CABAL   : ${INSTALL_CABAL}"
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
echo "SPLIT_OBJS      : ${SPLIT_OBJS}"
echo "DEVELOPMENT_MODE: ${DEVELOPMENT_MODE}"

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
if [ ${GHC_VERSION} = "head" ] ; then
    echo "Using GHC head."
elif [ "$(ghc --version | grep $GHC_VERSION)" ]; then
    echo "GHC version ${GHC_VERSION} found."
    which ghc
    ghc --version
elif [ $INSTALL_GHC = "yes" ]; then
    echo "GHC version ${GHC_VERSION} not found. Installing."
    if grep "${GHC_VERSION} ${PLATFORM} ${GMP_VERSION}" etc/build/ghc_urls.txt ; then
        GHC_TARBALL=$(grep "${GHC_VERSION} ${PLATFORM} ${GMP_VERSION}" etc/build/ghc_urls.txt | cut -d ' ' -f 4)
        URL="http://www.haskell.org/ghc/dist/${GHC_VERSION}/${GHC_TARBALL}"
        wget --no-check-certificate -c "${URL}"
        echo "Extracting..."
        case "${GHC_TARBALL}" in 
            *tar.bz2)
                tar --extract --file="${GHC_TARBALL}" --bzip2
                ;;
            *tar.xz)
                tar --extract --file="${GHC_TARBALL}" --xz
                ;;
            *)
                echo "Sorry, cannot extract: ${GHC_TARBALL}"
                exit 1
                ;;
        esac
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
else
    echo "GHC version ${GHC_VERSION} not found."
    echo "Either set INSTALL_GHC=yes or install it manually."
    exit 1
fi

# installing cabal-install
if [ "$(cabal --version | head -n 1 | grep ${CABAL_VERSION_CHECK})" ]; then
    echo "cabal-install version ${CABAL_VERSION_CHECK} found."
elif [ $INSTALL_CABAL = "yes" ]; then
    echo "cabal-install version ${CABAL_VERSION_CHECK} not found. Installing version ${CABAL_VERSION}."
    rm -rf cabal-install-tmp
    mkdir cabal-install-tmp
    pushd cabal-install-tmp
    wget --no-check-certificate -c "http://hackage.haskell.org/packages/archive/cabal-install/${CABAL_VERSION}/cabal-install-${CABAL_VERSION}.tar.gz"
    tar zxf "cabal-install-${CABAL_VERSION}.tar.gz"
    pushd "cabal-install-${CABAL_VERSION}"
    EXTRA_CONFIGURE_OPTS="" bash bootstrap.sh --user --no-doc
    popd
    popd
    rm -rf cabal-install-tmp
else
    echo "cabal-install version ${CABAL_VERSION_CHECK} not found."
    echo "Build will continue anyway."
    echo "If it doesn't work, either set INSTALL_CABAL=yes or install the right version manually."
fi

if [ $DEVELOPMENT_MODE = "yes" ]; then
    echo "Skipping: cabal update"
else
    cabal update
fi

ghc   --version
cabal --version

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
    PROFILING="--enable-library-profiling --enable-profiling"
else
    PROFILING="--disable-library-profiling --disable-profiling"
fi

if [ ${DYNAMIC} = "yes" ]; then
    DYNAMIC="--ghc-options \"-dynamic\""
else
    DYNAMIC=""
fi

if [ $SPLIT_OBJS = "yes" ]; then
    SPLIT_OBJS="--enable-split-objs"
else
    SPLIT_OBJS=""
fi

if ! [ $DEVELOPMENT_MODE = "yes" ]; then
    if [ ${GHC_VERSION} = "head" ] ; then
        rm -f cabal.config
    elif [ -f etc/hs-deps/cabal.config-${GHC_VERSION} ]; then
        cp etc/hs-deps/cabal.config-${GHC_VERSION} cabal.config
        echo "using a cabal freeze file: etc/hs-deps/cabal.config-${GHC_VERSION}"
    else
        rm -f cabal.config
        echo "not using a cabal freeze file: etc/hs-deps/cabal.config-${GHC_VERSION} not found"
    fi
fi

# install conjure, finally

cabal install                                                           \
    --only-dependencies                                                 \
    --force-reinstalls                                                  \
    --reorder-goals --max-backjumps 10000                               \
    ${SPLIT_OBJS}                                                       \
    ${DYNAMIC} ${PROFILING} ${TESTS} ${DOCS} ${LLVM} ${OPTIMISATION}    \
    --bindir="${BIN_DIR}" -j"${USE_CORES}"

cabal configure                                                         \
    ${SPLIT_OBJS}                                                       \
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
