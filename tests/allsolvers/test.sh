#!/bin/bash

# get the script directory
DIR="$( cd "$( dirname "$0" )" && pwd )"

(
cd $DIR
./run.sh | tee stdout
# cp stdout stdout.expected # uncomment to update the expected file
if ! diff stdout stdout.expected; then
    echo "The generated stdout doesn't match the expected stdout."
    echo "Test failed..."
    exit 1
else
    echo "Pass!"
fi
)
