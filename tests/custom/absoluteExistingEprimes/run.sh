
rm -rf conjure-output
conjure modelling test.essence --channelling=no -ax

echo ""
conjure solve test.essence --use-existing-models=model000001.eprime
rm -rf conjure-output/*.solution *.solution

echo ""
conjure solve test.essence --use-existing-models=model000002.eprime
rm -rf conjure-output/*.solution *.solution

mkdir -p some/other/dir
cp -r conjure-output/*.eprime some/other/dir/

echo ""
conjure solve test.essence --use-existing-models=some/other/dir/model000001.eprime -o conjure-output-1

echo ""
conjure solve test.essence --use-existing-models=some/other/dir/model000002.eprime -o conjure-output-2

# echo ""
# conjure solve test.essence --use-existing-models=${PWD}/some/other/dir/model000001.eprime -o conjure-output-2-1

# echo ""
# conjure solve test.essence --use-existing-models=${PWD}/some/other/dir/model000002.eprime -o conjure-output-2-2

rm -rf conjure-output-* *.solution *.stats.json* some

