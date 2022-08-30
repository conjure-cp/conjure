#!/bin/bash

set -o errexit
set -o nounset

if ${COVERAGE} && [ -z $DOWNLOADSECUREFILE_SECUREFILEPATH ]; then
    # this is from https://cloudblogs.microsoft.com/opensource/2019/04/05/publishing-github-pages-from-azure-pipelines
    mkdir ~/.ssh && mv $DOWNLOADSECUREFILE_SECUREFILEPATH ~/.ssh/id_rsa
    chmod 700 ~/.ssh && chmod 600 ~/.ssh/id_rsa
    ssh-keyscan -t rsa github.com >> ~/.ssh/known_hosts
    git clone git@github.com:conjure-cp/conjure-code-coverage.git

    # save this file for later
    cp conjure-code-coverage/latest/index.html latest-index.html

    # copy the report over to `latest`
    rm -rf conjure-code-coverage/latest
    mkdir -p conjure-code-coverage/latest
    cp -r .stack-work/install/*/*/*/hpc/combined/custom/* conjure-code-coverage/latest

    # rename the cryptic directory name for better diffs over time
    conjureDirName=$(cd conjure-code-coverage/latest ; ls | grep conjure-cp)
    mv conjure-code-coverage/latest/${conjureDirName} conjure-code-coverage/latest/conjure-cp

    # search & replace to fix links
    find conjure-code-coverage/latest -type f -exec sed -i "s/${conjureDirName}/conjure-cp/g" {} \;

    # remove the version for better diffs
    sed -i "s/repositoryVersion = &quot;.*&quot;/repositoryVersion = REMOVED/g" conjure-code-coverage/latest/conjure-cp/Conjure.RepositoryVersion.hs.html

    # move the index file back
    mv latest-index.html conjure-code-coverage/latest/index.html

    # copy latest
    TODAY=$(date "+%Y-%m-%d")
    rm -rf conjure-code-coverage/${TODAY}
    cp -r conjure-code-coverage/latest conjure-code-coverage/${TODAY}

    (
    cd conjure-code-coverage
    git config --local user.name "Özgür Akgün"
    git config --local user.email "ozgurakgun@gmail.com"

    rm -f */custom.tix

    git add --all ${TODAY}
    if [[ -n $(git status -s ${TODAY}) ]]; then
        git commit ${TODAY} -m "Conjure commit: https://github.com/conjure-cp/conjure/commit/${SOURCE_VERSION} (daily snapshot)"
    fi

    git add --all latest
    if [[ -n $(git status -s latest) ]]; then
        git commit latest -m "Conjure commit: https://github.com/conjure-cp/conjure/commit/${SOURCE_VERSION}"
    fi

    git push origin master
    )
else
    echo "Skipping, COVERAGE is set to ${COVERAGE}"
fi
