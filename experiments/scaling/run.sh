#!/bin/bash

parallel --eta --joblog joblog \
    "conjure --mode compact --in {2} --out {2.}-{1}.eprime +RTS -s 2> {2.}-{1}.stderr" \
        ::: $(seq -w 1 10) \
        ::: */*.essence
