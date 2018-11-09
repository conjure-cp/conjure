for d in ~/EssenceCatalog/problems/*; do
  if [ -d "$d" ]; then

    model=$(ls $d/*.essence | head -1)
    # echo $model
    params=$(find $d -type f -name "*.param")

    for p in $params; do
        echo $model
        echo $p
        newName=$(dirname $model)/$(basename $model .essence)~$(basename $p .param)
        conjureOutput=$newName
        mkdir $conjureOutput
        # echo $conjureOutput
        treeName=$conjureOutput/"out.json"
        # echo $treeName
        conjure solve $model $p -o $conjureOutput --copy-solutions=off --savilerow-options='-S0 -O0 -active-cse -reduce-domains -aggregate' --solver-options "-dumptreejson "$treeName
    done
  fi
done