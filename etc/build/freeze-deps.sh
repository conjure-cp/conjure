#/bin/bash

# create a dependency freeze file

set -o errexit
set -o nounset

SCRIPT_DIR="$( cd "$( dirname "$0" )" && pwd )"
source ${SCRIPT_DIR}/default_envvars.sh
echo "Creating a cabal-freeze file for GHC ${GHC_VERSION}"
git checkout -- cabal.config-*
cabal freeze --enable-tests
cat cabal.config | grep -v '             base ==' > cabal.config-${GHC_VERSION}
rm -f cabal.config
git diff cabal.config-${GHC_VERSION}
