#!/bin/bash

export DEVELOPMENT_MODE=${DEVELOPMENT_MODE:-"no"}

if [ $DEVELOPMENT_MODE = "yes" ]; then
    echo "Not touching the version file: DEVELOPMENT_MODE = yes"
else
    if ! [ -d .git ] && [ -f src/Conjure/RepositoryVersion.hs ] ; then
        echo "This is not a git repository, but it contains repository information. Reusing."
        cat src/Conjure/RepositoryVersion.hs | grep "repositoryVersion ="
    else
        VERSION="unknown"
        if [ -d .git ] ; then
            VERSION=$(git log -1 --pretty=format:"%h (%ai)")
        fi
        if ( grep "repositoryVersion = \"${VERSION}\"" src/Conjure/RepositoryVersion.hs 2> /dev/null > /dev/null ) ; then
            echo "Reusing src/Conjure/RepositoryVersion.hs with version ${VERSION}."
        else
            echo "Generating src/Conjure/RepositoryVersion.hs with version ${VERSION}."
            echo "module Conjure.RepositoryVersion where"   >  src/Conjure/RepositoryVersion.hs
            echo "import Prelude"                           >> src/Conjure/RepositoryVersion.hs
            echo "repositoryVersion :: String"              >> src/Conjure/RepositoryVersion.hs
            echo "repositoryVersion = \"${VERSION}\""       >> src/Conjure/RepositoryVersion.hs
        fi
    fi
fi

cat src/Conjure/RepositoryVersion.hs
