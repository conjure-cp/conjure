
conjure modelling sns.essence -ac --generate-neighbourhoods --frameUpdate=decomposition

# remove comments and move file
cat conjure-output/model000001.eprime | grep -v '\$' > sns-compact.eprime.new


# check output, save new output
diff sns-compact.eprime.new sns-compact.eprime
mv sns-compact.eprime.new sns-compact.eprime

rm -rf conjure-output

