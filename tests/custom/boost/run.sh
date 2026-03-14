#!/bin/bash

parallel --no-notice -k --tag --timeout 2 \
    'conjure boost {} > {.}.boost.essence ; conjure pretty {} > {.}.pretty.essence ; ! diff {.}.pretty.essence {.}.boost.essence || rm {.}.boost.essence' \
    ::: $(find * -name '*.essence' | grep -v 'boost.essence'  | grep -v 'pretty.essence' | LC_ALL=C sort)

git diff $(find * -name '*.essence')
