for p in params/*.param; do
    echo $p
    conjure features bacp.essence $p
    echo ""
done
