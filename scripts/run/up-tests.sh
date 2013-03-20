#!/bin/sh
# Runs the (~250) tests for converting the eprime solution back to essence

mkdir -p dist/upTests
ghc -O2 \
    -odir  dist/upTests \
    -hidir dist/upTests \
    -feager-blackholing  -threaded  -rtsopts --make \
    -isrc "src/Language/E/Up/testing/UpTests.hs"\
    -o dist/UpTestsBin && dist/UpTestsBin $@ +RTS -N6 -RTS;
