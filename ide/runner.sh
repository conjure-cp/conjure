for d in /scratch/EssenceCatalog/problems/*; do
  if [ -d "$d" ]; then

    model=$(ls $d/*.essence | head -1)
    # echo $model
    params=$(find $d -type f -name "*.param")

    count=0

    for p in $params; do
	((count++))
       # echo $model
        echo $p
        newName=$(dirname $model)/$(basename $model .essence)~$(basename $p .param)
        conjureOutput=$newName
        mkdir $conjureOutput
        # echo $conjureOutput
        treeName=$conjureOutput/"out.json"
        # echo $treeName

	#pwd &
        conjure solve $model $p -o $conjureOutput --copy-solutions=off --savilerow-options='-S0 -O3' --solver-options "-dumptreejson "$treeName &
	
	#echo $count

	if (($count > 20));	then
		echo "waiting!!!!!!!!!!!!!!!!!!!!!!!!"
		wait
		count=0
	fi
	
    done
  fi
done

