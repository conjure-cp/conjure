#!/bin/bash

# to be included (sourced) into other bash scripts

function download {
    if which curl 2> /dev/null > /dev/null; then
        curl -L -O $1
    elif which wget 2> /dev/null > /dev/null; then
        wget --no-check-certificate -c $1
    else
        echo "You seem to have neither curl nor wget on this computer."
        echo "Cannot download without one of them."
        exit 1
    fi
}
export -f download
