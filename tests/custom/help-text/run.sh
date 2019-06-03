pushd ../../../docs > /dev/null
make conjure-help
git diff conjure-help.txt
popd > /dev/null
