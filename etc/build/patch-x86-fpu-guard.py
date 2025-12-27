#!/usr/bin/env python3
import sys
from pathlib import Path


def patch_file(path: Path, replacement: str) -> None:
    if not path.exists():
        print(f"NOTE: {path} not found; skipping x86 guard patch.", file=sys.stderr)
        return

    text = path.read_text()
    if replacement in text:
        return
    if "defined(__linux__)" not in text:
        print(
            f"NOTE: did not find defined(__linux__) in {path}; skipping x86 guard.",
            file=sys.stderr,
        )
        return

    # Narrow the linux guard to x86 only for FPU control macros.
    path.write_text(text.replace("defined(__linux__)", replacement, 1))


def main(argv: list[str]) -> int:
    if not argv:
        print("Usage: patch-x86-fpu-guard.py <file> [file...]", file=sys.stderr)
        return 2

    replacement = "defined(__linux__) && (defined(__i386__) || defined(__x86_64__))"
    for arg in argv:
        patch_file(Path(arg), replacement)
    return 0


if __name__ == "__main__":
    raise SystemExit(main(sys.argv[1:]))
