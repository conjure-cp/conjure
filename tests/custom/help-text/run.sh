(
cd ../../../docs
make conjure-help 2>&1 | grep -v "Entering directory" | grep -v "Leaving directory"
git diff conjure-help.txt
)
