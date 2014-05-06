#!/bin/bash

runhaskell gen.hs
parallel "conjure --mode compact --in {} --out {.}.eprime +RTS -s 2> {.}.stderr" ::: *.essence
