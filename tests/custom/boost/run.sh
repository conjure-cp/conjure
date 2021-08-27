#!/bin/bash

parallel --no-notice -j1 --tag \
    'conjure boost {} > {.}.output.essence' \
    ::: $(find * -name '*.essence' | grep -v 'output.essence')
