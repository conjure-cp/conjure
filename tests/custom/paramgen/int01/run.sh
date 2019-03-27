
echo "$ conjure parameter-generator int.essence --essence-out int-gen.essence"
conjure parameter-generator int.essence --essence-out int-gen.essence
for file in int-gen.*; do
    echo "File: $file"
    cat $file
    echo "--------------------"
    echo ""
done

echo "$ conjure parameter-generator int.essence --essence-out int-gen.essence --MININT -10 --MAXINT 50"
conjure parameter-generator int.essence --essence-out int-gen.essence --MININT -10 --MAXINT 50
for file in int-gen.*; do
    echo "File: $file"
    cat $file
    echo "--------------------"
    echo ""
done

rm -f int-gen.*
