pushd ../../../docs > /dev/null
make conjure-help
rm conjure-help.html
popd > /dev/null
mv ../../../docs/conjure-help.txt .
git diff conjure-help.txt
