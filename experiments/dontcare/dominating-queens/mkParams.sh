#!/bin/bash

function one() {
    echo "language ESSENCE' 1.3" >  $1.param
    echo "letting n be $1"       >> $1.param
}

export -f one

parallel one ::: $(seq -w 4 15)
