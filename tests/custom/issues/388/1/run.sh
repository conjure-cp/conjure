
rm -rf conjure-output *.solution *.stats.json

conjure solve -ax --smart-filenames --channelling=no *.essence --validate-solutions --number-of-solutions=all

echo "----"
cat conjure-output/*.solution

echo "----"
cat conjure-output/*.solution | LC_ALL=C sort -u

rm -rf conjure-output *.solution *.stats.json

