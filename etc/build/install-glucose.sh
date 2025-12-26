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
import re

path = Path("simp/Main.cc")
text = path.read_text()
pattern = r"^\\s*_FPU_GETCW\\(oldcw\\);[^\\n]*$"
replacement = (
    "#if defined(__i386__) || defined(__x86_64__)\\n"
    "\\g<0>\\n"
    "#endif"
)
new_text, count = re.subn(pattern, replacement, text, flags=re.M)
if count > 0:
    path.write_text(new_text)
else:
    raise SystemExit("Expected _FPU_GETCW line not found in simp/Main.cc")
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
