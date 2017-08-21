
conjure pretty sns.essence > sns-pretty.essence.new

# check output, save new output
diff sns-pretty.essence sns-pretty.essence.new
mv sns-pretty.essence.new sns-pretty.essence

conjure -ac sns.essence

# remove comments and move file
cat conjure-output/model000001.eprime | grep -v '\$' > sns-compact.eprime.new


# check output, save new output
diff sns-compact.eprime.new sns-compact.eprime
mv sns-compact.eprime.new sns-compact.eprime

rm -rf conjure-output

