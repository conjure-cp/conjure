#!/bin/bash

set -o errexit
set -o nounset

if ${COVERAGE}; then
    # this is from https://cloudblogs.microsoft.com/opensource/2019/04/05/publishing-github-pages-from-azure-pipelines
    mkdir ~/.ssh && mv $DOWNLOADSECUREFILE_SECUREFILEPATH ~/.ssh/id_rsa
    chmod 700 ~/.ssh && chmod 600 ~/.ssh/id_rsa
    ssh-keyscan -t rsa github.com >> ~/.ssh/known_hosts
    git clone git@github.com:conjure-cp/conjure-code-coverage.git

    mkdir -p conjure-code-coverage/latest
    cp -r .stack-work/install/*/*/*/hpc/combined/custom/* conjure-code-coverage/latest

    TODAY=$(date "+%Y-%m-%d")
    mkdir -p conjure-code-coverage/${TODAY}
    cp -r .stack-work/install/*/*/*/hpc/combined/custom/* conjure-code-coverage/${TODAY}

    cd conjure-code-coverage
    git config --local user.name "Özgür Akgün"
    git config --local user.email "ozgurakgun@gmail.com"

    rm -f */custom.tix

    cp latest/index.html ${TODAY}/index.html
    git add ${TODAY}
    git commit ${TODAY} -m "Conjure commit: https://github.com/conjure-cp/conjure/commit/${SOURCE_VERSION} (daily snapshot)"

    git add latest
    git commit latest -m "Conjure commit: https://github.com/conjure-cp/conjure/commit/${SOURCE_VERSION}"

    git push origin master
else
    echo "Skipping, COVERAGE is set to ${COVERAGE}"
fi
