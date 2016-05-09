#/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}

wget -c http://savilerow.cs.st-andrews.ac.uk/savilerow-1.6.4-linux.tgz
tar zxvf savilerow-1.6.4-linux.tgz
cp savilerow-1.6.4-linux/savilerow.jar bin/savilerow.jar
mkdir -p bin/lib
cp savilerow-1.6.4-linux/lib/trove.jar bin/lib/trove.jar
pushd bin
echo '#!/bin/bash'                                                               >  savilerow
echo 'DIR="$( cd "$( dirname "$0" )" && pwd )"'                                  >> savilerow
echo 'java -ea -XX:ParallelGCThreads=1 -Xmx8G -jar "$DIR/savilerow.jar" "$@"'    >> savilerow
chmod +x savilerow
popd
rm -rf savilerow-1.6.4-linux.tgz savilerow-1.6.4-linux

