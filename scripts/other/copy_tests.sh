#!/bin/sh


if [ $# -eq 0 ]
then
  echo "Usage: `basename $0` frequency directory"
  echo " Copy all essence files in the current directory inculding subdirectories"
  echo "   to the specifed directory with the specifed frequency."
  exit 2
fi
set -o nounset
freq=$1
base=$2
for fp  in `ack -g essence `; do
	name="${fp%.*}"
	dir="${base}/${name}"
	mkdir "${dir}"
	cp "${fp}" "${dir}"
	echo "${freq}" > "${dir}/build_frequency"
done
