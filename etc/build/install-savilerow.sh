#!/bin/bash

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.cabal/bin}

rm -rf savilerow-repo savilerow*.tgz

######## this is the release
# wget -c http://savilerow.cs.st-andrews.ac.uk/savilerow-1.6.5-linux.tgz
# tar zxvf savilerow-1.6.5-linux.tgz
# mv savilerow-1.6.5-linux savilerow-repo

######## we are using an unreleased version...
wget --no-check-certificate -c https://ozgur.host.cs.st-andrews.ac.uk/SavileRows/savilerow-2017-08-16--508fb36e88cc-linux.tgz
tar zxvf savilerow-2017-08-16--508fb36e88cc-linux.tgz
mv savilerow-2017-08-16--508fb36e88cc-linux savilerow-repo

(cd savilerow-repo ; ./compile.sh)
cp savilerow-repo/savilerow.jar ${BIN_DIR}/savilerow.jar
mkdir -p ${BIN_DIR}/lib
cp savilerow-repo/lib/trove.jar ${BIN_DIR}/lib/trove.jar

rm -rf savilerow-repo savilerow*.tgz

echo '#!/bin/bash'                                                               >  ${BIN_DIR}/savilerow
echo 'DIR="$( cd "$( dirname "$0" )" && pwd )"'                                  >> ${BIN_DIR}/savilerow
echo 'java -ea -XX:ParallelGCThreads=1 -Xmx8G -jar "$DIR/savilerow.jar" "$@"'    >> ${BIN_DIR}/savilerow
chmod +x ${BIN_DIR}/savilerow
