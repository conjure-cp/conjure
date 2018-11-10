echo "bytes,seconds"
for d in ~/EssenceCatalog/problems/*; do
    if [ -d "$d" ]; then
        trees=$(find $d -type f -name "*.json")

        for tree in $trees; do

            t=$( { /usr/bin/time -f "%U" node ~/conjure/ide/out/benchmarker.js $tree; } 2>&1 )
            s=$(stat --printf="%s" $tree)
            
            echo $s,$t
        done

    fi
done
