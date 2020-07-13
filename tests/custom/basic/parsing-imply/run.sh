
for essence in *.essence; do
    echo ========================================
    echo $essence
    conjure pretty $essence > $essence.pretty
    diff $essence $essence.pretty
    conjure solve $essence --number-of-solutions=all --solutions-in-one-file
    cat conjure-output/model000001.solutions
done

rm -rf *.pretty conjure-output
