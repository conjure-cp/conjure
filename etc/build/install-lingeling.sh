#/bin/bash

set -o errexit
set -o nounset


rm -rf ~/tmp-install-lingeling
mkdir ~/tmp-install-lingeling
pushd ~/tmp-install-lingeling
wget -c http://fmv.jku.at/lingeling/lingeling-ayv-86bf266-140429.zip
unzip lingeling-ayv-86bf266-140429.zip
./build.sh
mkdir -p ~/bin
cp binary/lingeling ~/bin/lingeling
echo "lingeling executable is at ~/bin/lingeling"
ls -l ~/bin/lingeling
popd
rm -rf ~/tmp-install-lingeling

