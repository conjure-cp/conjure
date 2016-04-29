#/bin/bash

set -o errexit
set -o nounset


rm -rf ~/tmp-install-lingeling
mkdir ~/tmp-install-lingeling
pushd ~/tmp-install-lingeling
wget -c http://fmv.jku.at/lingeling/lingeling-ayv-86bf266-140429.zip
unzip lingeling-ayv-86bf266-140429.zip
./build.sh
cp binary/lingeling ~/bin/lingeling
popd
rm -rf ~/tmp-install-lingeling

