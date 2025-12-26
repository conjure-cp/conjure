#!/bin/bash

# version as of 26 Feb 2024
VERSION=4.2.1

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}

# QEMU+arm64 toolchains can treat Glucose's class-memaccess warnings as errors.
export CXXFLAGS="${CXXFLAGS:-} -Wno-class-memaccess -Wno-error=class-memaccess"

rm -rf tmp-install-glucose
mkdir tmp-install-glucose
pushd tmp-install-glucose
git clone https://github.com/audemard/glucose.git
cd glucose
git checkout $VERSION
python3 - <<'PY'
from pathlib import Path
import sys

targets = [Path("simp/Main.cc"), Path("parallel/Main.cc")]
replacement = "defined(__linux__) && (defined(__i386__) || defined(__x86_64__))"

for path in targets:
    text = path.read_text()
    if replacement in text:
        continue
    if "defined(__linux__)" not in text:
        print(f"NOTE: did not find defined(__linux__) in {path}; skipping x86 guard.", file=sys.stderr)
        continue
    path.write_text(text.replace("defined(__linux__)", replacement, 1))
PY
echo 'CXXFLAGS += -Wno-class-memaccess -Wno-error=class-memaccess' >> mtl/template.mk
(
    cd simp
    make -j${PROCESSES} r
    cp glucose_release ${BIN_DIR}/glucose
    echo "glucose executable is at ${BIN_DIR}/glucose"
    ls -l ${BIN_DIR}/glucose
)
(
    cd parallel
    make -j${PROCESSES} r
    cp glucose-syrup_release ${BIN_DIR}/glucose-syrup
    echo "glucose-syrup executable is at ${BIN_DIR}/glucose-syrup"
    ls -l ${BIN_DIR}/glucose-syrup
)
popd
rm -rf tmp-install-glucose
