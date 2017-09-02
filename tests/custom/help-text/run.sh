pushd ../../../docs > /dev/null
make conjure-help
git diff conjure-help.txt conjure-help.html
popd > /dev/null
