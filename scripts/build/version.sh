#/bin/bash

if ! [ -d .hg ] && [ -f src/Conjure/RepositoryVersion.hs ] ; then
    echo "This is not a mercurial repository, but it contains repository information. Reusing."
    cat src/Conjure/RepositoryVersion.hs | grep "repositoryVersion ="
else
    VERSION="unknown"
    if [ -d .hg ] ; then
        VERSION=$(hg log -l1 --template "{node|short} ({date|isodate})")
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


