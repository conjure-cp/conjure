#!/bin/bash

# Assuming the breaking_conjure repo is at: ~/repos/stacs_cp/breaking_conjure

(
    # remove comments (because JSON structure may have changed...)
    cd ~/repos/stacs_cp/breaking_conjure/fixed
    find * -name '*.essence'         | parallel "[ -f {} ] && (cat {} | grep -v '\\$' > {}.temp ; mv {}.temp {})"
    find * -name '*.param'           | parallel "[ -f {} ] && (cat {} | grep -v '\\$' > {}.temp ; mv {}.temp {})"
    find * -name '*.solution'        | parallel "[ -f {} ] && (cat {} | grep -v '\\$' > {}.temp ; mv {}.temp {})"
    find * -name '*.eprime'          | parallel "[ -f {} ] && (cat {} | grep -v '\\$' > {}.temp ; mv {}.temp {})"
    find * -name '*.eprime-param'    | parallel "[ -f {} ] && (cat {} | grep -v '\\$' > {}.temp ; mv {}.temp {})"
    find * -name '*.eprime-solution' | parallel "[ -f {} ] && (cat {} | grep -v '\\$' > {}.temp ; mv {}.temp {})"
)

find ~/repos/stacs_cp/breaking_conjure/fixed -name '*.essence'         | parallel -j1 'mkdir -p $(md5 -q {}) ; cp {} $(md5 -q {})/$(md5 -q {}).essence'
find ~/repos/stacs_cp/breaking_conjure/fixed -name '*.param'           | parallel -j1 'mkdir -p $(md5 -q {}) ; cp {} $(md5 -q {})/$(md5 -q {}).param.essence'
find ~/repos/stacs_cp/breaking_conjure/fixed -name '*.solution'        | parallel -j1 'mkdir -p $(md5 -q {}) ; cp {} $(md5 -q {})/$(md5 -q {}).solution.essence'
find ~/repos/stacs_cp/breaking_conjure/fixed -name '*.eprime'          | parallel -j1 'mkdir -p $(md5 -q {}) ; cp {} $(md5 -q {})/$(md5 -q {}).eprime.essence'
find ~/repos/stacs_cp/breaking_conjure/fixed -name '*.eprime-param'    | parallel -j1 'mkdir -p $(md5 -q {}) ; cp {} $(md5 -q {})/$(md5 -q {}).eprime-param.essence'
find ~/repos/stacs_cp/breaking_conjure/fixed -name '*.eprime-solution' | parallel -j1 'mkdir -p $(md5 -q {}) ; cp {} $(md5 -q {})/$(md5 -q {}).eprime-solution.essence'
