#!/bin/bash

# WMaxCDCL (MSE 2023 solver source archive)
# version: as packaged by MaxSAT Evaluation 2023 (archive does not expose a semantic version)

source "download.sh" 2> /dev/null               # if called from the script dir
source "etc/build/download.sh" 2> /dev/null     # if called from the repo base (the common case)

set -o errexit
set -o nounset

export BIN_DIR=${BIN_DIR:-${HOME}/.local/bin}
export PROCESSES=${PROCESSES:-1}

SCRIPT_DIR=$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)
REPO_ROOT=$(cd "${SCRIPT_DIR}/../.." && pwd)

LOCAL_WMAXCDCL_DIR="${REPO_ROOT}/WMaxCDCL"
LOCAL_WMAXCDCL_ZIP="${REPO_ROOT}/WMaxCDCL.zip"
URL="https://maxsat-evaluations.github.io/2023/mse23-solver-src/exact/WMaxCDCL.zip"

rm -rf tmp-install-wmaxcdcl
mkdir -p tmp-install-wmaxcdcl
pushd tmp-install-wmaxcdcl
download "${URL}" WMaxCDCL.zip
unzip WMaxCDCL.zip

################################################################################
# WMaxCDCL's DIMACS parser carries unused locals that break some arm64 toolchains.
python3 - <<'PY'
from pathlib import Path
import re
import sys

path = Path("WMaxCDCL/code/core/Dimacs.h")
if not path.exists():
    print("NOTE: WMaxCDCL/code/core/Dimacs.h not found; skipping patch.", file=sys.stderr)
    raise SystemExit(0)

text = path.read_text()
start = text.find("static void parse_DIMACS_new")
if start == -1:
    print("NOTE: parse_DIMACS_new not found; skipping patch.", file=sys.stderr)
    raise SystemExit(0)

end = text.find("\n    template<class Solver>", start)
if end == -1:
    print("NOTE: parse_DIMACS_new end not found; skipping patch.", file=sys.stderr)
    raise SystemExit(0)

segment = text[start:end]
original = segment
segment = re.sub(r"\\bint\\s+vars\\s*=\\s*0\\s*;\\s*", "", segment, count=1)
segment = re.sub(r"\\bint\\s+clauses\\s*=\\s*0\\s*;\\s*", "", segment, count=1)
segment = re.sub(r"\\bint\\s+cnt\\s*=\\s*0\\s*;\\s*", "", segment, count=1)
segment = re.sub(r"\\bcnt\\s*\\+\\+\\s*;\\s*", "", segment, count=1)

if segment != original:
    text = text[:start] + segment + text[end:]
    path.write_text(text)
PY
################################################################################

################################################################################
# WMaxCDCL uses x86-only FPU control macros; guard them on x86 Linux only.
python3 - <<'PY'
from pathlib import Path
import sys

path = Path("WMaxCDCL/code/simp/Main.cc")
if not path.exists():
    print("NOTE: WMaxCDCL/code/simp/Main.cc not found; skipping FPU guard patch.", file=sys.stderr)
    raise SystemExit(0)

text = path.read_text()
old = "#if defined(__linux__)"
new = "#if defined(__linux__) && (defined(__i386__) || defined(__x86_64__))"
if new in text:
    raise SystemExit(0)
if old not in text:
    print("NOTE: linux guard not found in Main.cc; skipping FPU guard patch.", file=sys.stderr)
    raise SystemExit(0)

path.write_text(text.replace(old, new, 1))
PY
################################################################################

cd WMaxCDCL/code/simp
make -j${PROCESSES}
mkdir -p "${BIN_DIR}"
cp -f wmaxcdcl "${BIN_DIR}/wmaxcdcl"
echo "WMaxCDCL executable is at ${BIN_DIR}/wmaxcdcl"
ls -l "${BIN_DIR}/wmaxcdcl"
popd
rm -rf tmp-install-wmaxcdcl
