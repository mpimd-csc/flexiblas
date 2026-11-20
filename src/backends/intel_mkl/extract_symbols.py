#!/usr/bin/env python3
"""
extract_symbols.py

Extracts the public symbols (functions/subroutines) of the LAPACK reference
sources:

  * BLAS      -- Fortran BLAS   (BLAS/SRC/*.f, *.f90)
  * LAPACK    -- Fortran LAPACK (SRC/*.f, *.f90)
  * CBLAS     -- C interface to BLAS   (CBLAS/include/cblas.h.in)
  * LAPACKE   -- C interface to LAPACK (LAPACKE/include/lapacke.h,
                                          LAPACKE/include/lapacke_utils.h)

Additionally it builds the list of symbols of the Index-64 ("ILP64") extended
API, i.e. all of the above with the "_64" suffix:

  * Fortran 64-bit routines are generated from the Fortran sources with the
    suffix "_64" (see CMAKE/GenerateSuffixedSource.cmake, SUFFIX "_64"),
    e.g. dgemm -> dgemm_64   (external linker symbol: dgemm_64_ with the
    usual Fortran name mangling).
  * CBLAS 64-bit prototypes are listed in  CBLAS/include/cblas_64.h
  * LAPACKE 64-bit prototypes are listed in LAPACKE/include/lapacke_64.h

Outputs (in --outdir, default ./symbols):
  blas_symbols.txt        lapack_symbols.txt
  cblas_symbols.txt       lapacke_symbols.txt
  index64/blas_64_symbols.txt      index64/lapack_64_symbols.txt
  index64/cblas_64_symbols.txt     index64/lapacke_64_symbols.txt
  index64/index64_symbols.txt      (fortran + cblas + lapacke, all "_64"-suffixed)
  all_symbols.json        (everything, with source/deprecated metadata)
"""

import argparse
import json
import re
import sys
from pathlib import Path

# --------------------------------------------------------------------------
# Fortran source scanning
# --------------------------------------------------------------------------

# Lines to ignore in Fortran sources: f90-style comments, fixed-form comments
# (C or * in column 1), preprocessor directives.
FORTRAN_COMMENT_RE = re.compile(r"^\s*(!.*)?$")
FORTRAN_FIXED_COMMENT_RE = re.compile(r"^[cC*dD]")
FORTRAN_PP_RE = re.compile(r"^\s*#")

# Matches e.g.
#   subroutine DLASSQ( n, x, incx, scale, sumsq )
#   pure subroutine foo(...)
#   real function SDSDOT(N,SX,INCX,SB,S)
#   DOUBLE PRECISION FUNCTION DNRM2 ( N, X, INCX )
#   complex*16 function zdotc(...)
#   recursive function something(...) result(res)
FORTRAN_SUBROUTINE_RE = re.compile(
    r"^\s*(?:(?:pure|elemental|recursive|impure|module)\s+)*subroutine\s+([A-Za-z]\w*)",
    re.IGNORECASE,
)
FORTRAN_FUNCTION_RE = re.compile(
    r"^\s*"
    r"(?:(?:pure|elemental|recursive|impure|module)\s+|"
    r"(?:double\s+precision|double\s+complex|complex\s*\*\s*\d+|"
    r"real\s*\*\s*\d+|integer\s*\*\s*\d+|"
    r"real|integer|complex|logical|character)\s+)*"
    r"function\s+([A-Za-z]\w*)",
    re.IGNORECASE,
)

# Intrinsic/technical routines that are not part of the public API.
FORTRAN_EXCLUDE = {
    "xerbla",        # error handler (publicly documented but internal)
    "xerbla_array",
    "lsame",         # internal comparison function
    "ilaenv",
    "iparmq",
    "iparam2stage",
    "ieeeck",
    "ilaenv2stage",
    "dlabad",
    "dladiv",
    "dlaruv",
    "dlamch",        # INSTALL/ machine constants
}


def scan_fortran_dir(directory: Path, deprecated: bool = False):
    """Return (names, deprecated_names) found in Fortran sources of a dir."""
    names = set()
    dep_names = set()
    if not directory.is_dir():
        return names, dep_names
    for path in sorted(directory.rglob("*")):
        if path.suffix.lower() not in (".f", ".f90"):
            continue
        # VARIANTS are alternative implementations, not part of the standard API
        if "VARIANTS" in path.parts:
            continue
        is_dep = (not deprecated) and "DEPRECATED" in path.parts
        try:
            text = path.read_text(errors="replace")
        except OSError as exc:
            print(f"warning: cannot read {path}: {exc}", file=sys.stderr)
            continue
        for line in text.splitlines():
            if FORTRAN_COMMENT_RE.match(line) or FORTRAN_PP_RE.match(line):
                continue
            if line and FORTRAN_FIXED_COMMENT_RE.match(line[0]):
                continue
            m = FORTRAN_SUBROUTINE_RE.match(line) or FORTRAN_FUNCTION_RE.match(line)
            if m:
                name = m.group(1).lower()
                if is_dep:
                    dep_names.add(name)
                else:
                    names.add(name)
    return names, dep_names


# --------------------------------------------------------------------------
# C header scanning
# --------------------------------------------------------------------------

def strip_c_comments(text: str) -> str:
    text = re.sub(r"/\*.*?\*/", " ", text, flags=re.DOTALL)
    text = re.sub(r"//[^\n]*", " ", text)
    return text


def c_header_statements(text: str):
    """Yield declaration statements of a (comment-stripped) C header."""
    # Drop preprocessor lines (includes, defines, conditionals)
    text = "\n".join(
        line for line in text.splitlines() if not line.lstrip().startswith("#")
    )
    # Split into statements terminated by ';' -- prototypes, enums, typedefs.
    for stmt in text.split(";"):
        stmt = stmt.strip()
        if stmt:
            yield stmt


# First identifier followed by '(' in a declaration = the function name.
C_FUNC_NAME_RE = re.compile(r"([A-Za-z_]\w*)\s*\(")

C_SYMBOL_RE = re.compile(r"^(cblas_[a-z0-9_]+|LAPACKE_[A-Za-z0-9_]+)$")


def scan_c_header(path: Path, prefix_re: re.Pattern):
    """Extract symbol names matching prefix_re from a C header."""
    names = set()
    if not path.is_file():
        print(f"warning: header not found: {path}", file=sys.stderr)
        return names
    text = strip_c_comments(path.read_text(errors="replace"))
    for stmt in c_header_statements(text):
        # Skip typedefs of function pointers etc.
        if re.match(r"^\s*(typedef|enum|struct|union)\b", stmt):
            continue
        # Only consider declarations that look like functions
        m = C_FUNC_NAME_RE.search(stmt)
        if not m:
            continue
        name = m.group(1)
        if prefix_re.fullmatch(name):
            names.add(name)
    return names


# --------------------------------------------------------------------------
# Main
# --------------------------------------------------------------------------

def main() -> int:
    parser = argparse.ArgumentParser(
        description="Extract BLAS / CBLAS / LAPACK / LAPACKE (and Index-64 API) symbols."
    )
    parser.add_argument(
        "--repo", default=str(Path(__file__).resolve().parent),
        help="path to the lapack source tree (default: script location)",
    )
    parser.add_argument(
        "--outdir", default=None,
        help="output directory (default: <repo>/symbols)",
    )
    parser.add_argument(
        "--include-deprecated", action="store_true",
        help="also list routines from the DEPRECATED directories as regular API",
    )
    args = parser.parse_args()

    repo = Path(args.repo).resolve()
    outdir = Path(args.outdir) if args.outdir else repo / "symbols"
    outdir.mkdir(parents=True, exist_ok=True)

    include_dep = args.include_deprecated

    # --- Fortran BLAS / LAPACK -------------------------------------------
    blas, blas_dep = scan_fortran_dir(repo / "BLAS" / "SRC", deprecated=include_dep)
    lapack, lapack_dep = scan_fortran_dir(repo / "SRC", deprecated=include_dep)
    blas -= blas_dep
    lapack -= lapack_dep
    blas -= FORTRAN_EXCLUDE
    lapack -= FORTRAN_EXCLUDE

    # --- CBLAS / LAPACKE (default API) -----------------------------------
    cblas = scan_c_header(
        repo / "CBLAS" / "include" / "cblas.h.in",
        re.compile(r"cblas_[a-z0-9_]+"),
    )
    # cblas.h.in contains internal helpers that are not part of the API
    cblas -= {"cblas_xerbla", "cblas_errprn"}
    lapacke = scan_c_header(
        repo / "LAPACKE" / "include" / "lapacke.h",
        re.compile(r"LAPACKE_[A-Za-z0-9_]+"),
    )
    lapacke |= scan_c_header(
        repo / "LAPACKE" / "include" / "lapacke_utils.h",
        re.compile(r"LAPACKE_[A-Za-z0-9_]+"),
    )

    # --- Index-64 extended API -------------------------------------------
    # Fortran: same routines, name + "_64" (GenerateSuffixedSource, SUFFIX "_64").
    index64 = {name + "_64" for name in (blas | lapack)}
    # CBLAS / LAPACKE 64: explicit prototypes in the *_64.h headers.
    cblas_64 = scan_c_header(
        repo / "CBLAS" / "include" / "cblas_64.h",
        re.compile(r"cblas_[a-z0-9_]+"),
    )
    cblas_64 -= {"cblas_xerbla", "cblas_errprn"}
    lapacke_64 = scan_c_header(
        repo / "LAPACKE" / "include" / "lapacke_64.h",
        re.compile(r"LAPACKE_[A-Za-z0-9_]+"),
    )
    index64 |= cblas_64 | lapacke_64

    def write_list(filename: str, names):
        with open(outdir / filename, "w") as fh:
            for name in sorted(names):
                fh.write(name + "\n")

    write_list("blas_symbols.txt", blas)
    write_list("lapack_symbols.txt", lapack)
    write_list("cblas_symbols.txt", cblas)
    write_list("lapacke_symbols.txt", lapacke)

    # --- Index-64 API, one file per API plus a combined list --------------
    index64_dir = outdir / "index64"
    index64_dir.mkdir(parents=True, exist_ok=True)
    write_list(str(index64_dir / "blas_64_symbols.txt"),
               {name + "_64" for name in blas})
    write_list(str(index64_dir / "lapack_64_symbols.txt"),
               {name + "_64" for name in lapack})
    write_list(str(index64_dir / "cblas_64_symbols.txt"), cblas_64)
    write_list(str(index64_dir / "lapacke_64_symbols.txt"), lapacke_64)
    write_list(str(index64_dir / "index64_symbols.txt"), index64)

    payload = {
        "blas": sorted(blas),
        "blas_deprecated": sorted(blas_dep),
        "lapack": sorted(lapack),
        "lapack_deprecated": sorted(lapack_dep),
        "cblas": sorted(cblas),
        "lapacke": sorted(lapacke),
        "index64": {
            "fortran": sorted(name + "_64" for name in (blas | lapack)),
            "cblas_64": sorted(cblas_64),
            "lapacke_64": sorted(lapacke_64),
            "all": sorted(index64),
        },
    }
    with open(outdir / "all_symbols.json", "w") as fh:
        json.dump(payload, fh, indent=2)

    print(f"BLAS    (Fortran)        : {len(blas):5d} symbols")
    print(f"LAPACK  (Fortran)        : {len(lapack):5d} symbols")
    print(f"CBLAS   (C)              : {len(cblas):5d} symbols")
    print(f"LAPACKE (C)              : {len(lapacke):5d} symbols")
    print(f"Index-64 API (all _64)   : {len(index64):5d} symbols")
    print(f"  - Fortran *_64         : {len(blas | lapack):5d}")
    print(f"  - cblas_*_64           : {len(cblas_64):5d}")
    print(f"  - LAPACKE_*_64         : {len(lapacke_64):5d}")
    if blas_dep or lapack_dep:
        print(f"deprecated (excluded)    : {len(blas_dep | lapack_dep):5d}")
    print(f"\nOutput written to {outdir}")
    return 0


if __name__ == "__main__":
    sys.exit(main())
