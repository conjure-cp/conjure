#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

if [ "$#" -ne 2 ]; then
    echo "Usage: $0 <artefacts-dir> <bundle-dir>" >&2
    exit 1
fi

ARTEFACTS_DIR=$1
BUNDLE_DIR=$2

if [ ! -d "${ARTEFACTS_DIR}" ]; then
    echo "Artefacts directory not found: ${ARTEFACTS_DIR}" >&2
    exit 1
fi

write_wrapper_common() {
    cat <<'EOF'
#!/bin/bash

set -o errexit
set -o nounset
set -o pipefail

SELF_DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" && pwd )"

prepend_env_dir() {
    local dir=$1
    local var_name=$2
    local current_value=${!var_name:-}
    case ":${current_value}:" in
        *":${dir}:"*) return 0 ;;
    esac
    if [ -n "${current_value}" ]; then
        export "${var_name}=${dir}:${current_value}"
    else
        export "${var_name}=${dir}"
    fi
}

prepend_env_dir "${SELF_DIR}" PATH

if [ -d "${SELF_DIR}/lib" ]; then
    case "$(uname -s)" in
        Darwin)
            prepend_env_dir "${SELF_DIR}/lib" DYLD_LIBRARY_PATH
            prepend_env_dir "${SELF_DIR}/lib" DYLD_FALLBACK_LIBRARY_PATH
            ;;
        Linux)
            prepend_env_dir "${SELF_DIR}/lib" LD_LIBRARY_PATH
            ;;
    esac
fi

if [ -d "${SELF_DIR}/share/minizinc" ] && [ -z "${MZN_STDLIB_DIR:-}" ]; then
    export MZN_STDLIB_DIR="${SELF_DIR}/share/minizinc"
fi
EOF
}

write_binary_wrapper() {
    local wrapper_path=$1
    local target_name=$2
    {
        write_wrapper_common
        cat <<EOF

exec "\${SELF_DIR}/libexec/${target_name}" "\$@"
EOF
    } > "${wrapper_path}"
    chmod 0755 "${wrapper_path}"
}

write_savilerow_wrapper() {
    local wrapper_path=$1
    {
        write_wrapper_common
        cat <<'EOF'

SAVILEROW_JAR="${SELF_DIR}/libexec/savilerow.jar"
TROVE_JAR="${SELF_DIR}/lib/trove.jar"

has_cgroups=false
for arg in "$@"; do
    if [ "${arg}" = "-cgroups" ]; then
        has_cgroups=true
        break
    fi
done

if [ "$(uname -s)" = "Darwin" ]; then
    if ${has_cgroups}; then
        echo "Ignoring -cgroups on macOS"
    fi
    exec java -ea -XX:ParallelGCThreads=1 \
        -Djava.library.path="${SELF_DIR}/lib/" \
        -Xmx8G \
        -cp "${SAVILEROW_JAR}:${TROVE_JAR}" \
        savilerow.EPrimeTailor "$@"
fi

if ${has_cgroups}; then
    TIMESTAMP=$(date +%s)
    GROUP_NAME="savilerow_${TIMESTAMP}_$$"

    echo "Using cgroups, group name: $GROUP_NAME"

    cgcreate -g cpu:/${GROUP_NAME}
    cgset -r cpu.cfs_quota_us=1000000 "${GROUP_NAME}"
    cgset -r cpu.cfs_period_us=1000000 "${GROUP_NAME}"
    cgexec -g cpu:${GROUP_NAME} \
        java -ea -XX:+UseG1GC -XX:ParallelGCThreads=1 \
        -Djava.library.path="${SELF_DIR}/lib/" \
        -Xmx8G \
        -cp "${SAVILEROW_JAR}:${TROVE_JAR}" \
        savilerow.EPrimeTailor "$@"
    cgdelete -g cpu:/${GROUP_NAME}
else
    exec java -ea -XX:+UseG1GC -XX:ParallelGCThreads=1 \
        -Djava.library.path="${SELF_DIR}/lib/" \
        -Xmx8G \
        -cp "${SAVILEROW_JAR}:${TROVE_JAR}" \
        savilerow.EPrimeTailor "$@"
fi
EOF
    } > "${wrapper_path}"
    chmod 0755 "${wrapper_path}"
}

write_readme() {
    cat <<'EOF' > "${BUNDLE_DIR}/README.txt"
This release bundle is relocatable.

Run tools directly from this directory, for example:
  ./conjure --help
  ./conjure solve problem.essence

You can also add this directory to PATH and use the commands normally.

The top-level executables are wrappers. They keep solver lookup, shared-library
search paths, and the MiniZinc standard library rooted inside the extracted
bundle, so the release can be moved without editing environment variables.
EOF
}

rm -rf "${BUNDLE_DIR}"
mkdir -p "${BUNDLE_DIR}/libexec"

shopt -s nullglob dotglob
for artefact_path in "${ARTEFACTS_DIR}"/*; do
    artefact_name=$(basename "${artefact_path}")
    case "${artefact_name}" in
        lib|share)
            cp -a "${artefact_path}" "${BUNDLE_DIR}/"
            ;;
        *)
            cp -a "${artefact_path}" "${BUNDLE_DIR}/libexec/"
            ;;
    esac
done
shopt -u nullglob dotglob

find "${BUNDLE_DIR}/libexec" -maxdepth 1 -type f -perm -u+x | while read -r executable_path; do
    executable_name=$(basename "${executable_path}")
    if [ "${executable_name}" = "savilerow" ]; then
        write_savilerow_wrapper "${BUNDLE_DIR}/savilerow"
    else
        write_binary_wrapper "${BUNDLE_DIR}/${executable_name}" "${executable_name}"
    fi
done

write_readme
