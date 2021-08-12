conjure parameter-generator bacp.essence

git diff bacp-instanceGenerator.essence
git diff bacp-instanceGenerator.essence.irace
git diff bacp-instanceRepair.essence

for p in params/*.param; do
    echo $p
    conjure features bacp.essence $p
    echo ""
done
