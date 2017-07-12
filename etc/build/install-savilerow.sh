#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}

# wget -c http://savilerow.cs.st-andrews.ac.uk/savilerow-1.6.4-linux.tgz
# tar zxvf savilerow-1.6.4-linux.tgz
# # cp savilerow-1.6.4-linux/savilerow.jar ${BIN_DIR}/savilerow.jar
# mkdir -p ${BIN_DIR}/lib
# cp savilerow-1.6.4-linux/lib/trove.jar ${BIN_DIR}/lib/trove.jar
# rm -rf savilerow-1.6.4-linux.tgz savilerow-1.6.4-linux

pushd ${BIN_DIR}
echo '#!/bin/bash'                                                               >  savilerow
echo 'DIR="$( cd "$( dirname "$0" )" && pwd )"'                                  >> savilerow
echo 'java -ea -XX:ParallelGCThreads=1 -Xmx8G -jar "$DIR/savilerow.jar" "$@"'    >> savilerow
chmod +x savilerow
popd

# using an unreleased version of savilerow
# NOTE: don't forget to update the version check in the travis file!
wget --no-check-certificate -c https://ozgur.host.cs.st-andrews.ac.uk/SavileRows/2017-07-03--ad9229da27ca/savilerow.jar
cp savilerow.jar ${BIN_DIR}/savilerow.jar
