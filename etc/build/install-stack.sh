#!/bin/bash

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export CI=${CI:-false}

function dlStack {
    local os arch platform url
    os="$(uname -s)"
    arch="$(uname -m)"

    case "$os" in
        Darwin) platform="osx" ;;
        Linux) platform="linux" ;;
        *) return 1 ;;
    esac

    case "$arch" in
        x86_64|amd64) arch="x86_64" ;;
        arm64|aarch64) arch="aarch64" ;;
        *) return 1 ;;
    esac

    mkdir -p "${BIN_DIR}"
    url="https://get.haskellstack.org/stable/${platform}-${arch}.tar.gz"
    tmp_dir="$(mktemp -d)"
    tarball="$(basename "${url}")"
    extract_dir="${tmp_dir}/extract"
    (
        cd "${tmp_dir}"
        download "${url}"
    )
    mkdir -p "${extract_dir}"
    tar xzf "${tmp_dir}/${tarball}" -C "${extract_dir}"
    stack_path="$(find "${extract_dir}" -type f -name stack | head -n 1)"
    if [ -z "${stack_path}" ]; then
        echo "Downloaded stack bindist but could not find the stack binary."
        return 1
    fi
    install -m 0755 "${stack_path}" "${BIN_DIR}/stack"
    rm -rf "${tmp_dir}"
}
export -f dlStack

stack_bin="${BIN_DIR}/stack"

if ! which stack 2> /dev/null > /dev/null && [ ! -x "${stack_bin}" ]; then
    echo "Installing Haskell build tool stack to ${BIN_DIR}"
    if dlStack; then
        echo "Downloaded stack."
    else
        echo "Couldn't download a stack bindist for this OS/arch."
        echo "This installer only supports prebuilt bindists (no source builds)."
        exit 1
    fi
fi

if ! which stack 2> /dev/null > /dev/null; then
    if [ -x "${stack_bin}" ]; then
        echo "Stack was installed to ${BIN_DIR}, but BIN_DIR is not on PATH."
        exit 1
    fi
    echo "Stack is not on PATH."
    exit 1
fi

ls -l "$(which stack)"
stack --version
