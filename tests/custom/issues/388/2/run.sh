
rm -rf conjure-output

conjure solve -ax --smart-filenames --channelling=no *.essence --validate-solutions

echo "----"
cat conjure-output/*.solution

echo "----"
cat conjure-output/*.solution | sort -u

rm -rf conjure-output *.solution

