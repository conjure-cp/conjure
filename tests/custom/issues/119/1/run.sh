
rm -rf conjure-output

conjure solve -ax --smart-filenames --channelling=no *.essence --validate-solutions --representations-auxiliaries=c

echo "----"
cat conjure-output/*.solution

echo "----"

cat conjure-output/*.solution | grep -v "^[$]" | LC_ALL=C sort -u

rm -rf conjure-output *.solution

