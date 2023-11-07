#!/bin/bash

# get the script directory
DIR="$( cd "$( dirname "$0" )" && pwd )"

(
cd $DIR
./run.sh | tee stdout
if ! diff stdout stdout.expected; then
    echo "The generated stdout doesn't match the expected stdout."
    echo "That's what we call a failed test around here..."
else
    echo "Pass!"
fi
)
