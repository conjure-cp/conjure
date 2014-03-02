#/bin/bash

if ! [ -d .hg ] && [ -f src/RepositoryVersion.hs ] ; then
    echo "This is not a mercurial repository, but it contains repository information. Reusing."
    cat src/RepositoryVersion.hs | grep "repositoryVersion ="
else
    VERSION="unknown"
    if [ -d .hg ] ; then
        VERSION=$(hg id -i | head -n 1)
    fi
    if ( grep "repositoryVersion = \"${VERSION}\"" src/RepositoryVersion.hs 2> /dev/null > /dev/null ) ; then
        echo "Reusing src/RepositoryVersion.hs with version ${VERSION}."
    else
        echo "Generating src/RepositoryVersion.hs with version ${VERSION}."
        echo "module RepositoryVersion where"       >  src/RepositoryVersion.hs
        echo "repositoryVersion :: String"          >> src/RepositoryVersion.hs
        echo "repositoryVersion = \"${VERSION}\""   >> src/RepositoryVersion.hs
    fi
fi


