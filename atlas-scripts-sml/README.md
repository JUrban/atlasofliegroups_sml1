# Atlas SML Port (Poly/ML)

## What exists so far

This repo currently ships Atlas as a C++ library plus the `atlas` interactive interpreter. The goal here is to run Atlas computations from Standard ML (Poly/ML) so we can translate `atlas-scripts/*.at` into `atlas-scripts-sml/*.sml` without depending on `.at` scripts.

As a first step, there is now a working Poly/ML FFI proof-of-concept that:

- Builds a small C ABI wrapper as a shared library (`.so`) linked against Atlas C++ object code.
- Calls into that shared library from SML using Poly/ML’s `Foreign` interface.
- Constructs Atlas groups from SML (currently “single simple factor” only).
- Constructs Atlas parameters from SML (from `(x,lambda,nu)` data).
- Loads `atlas-scripts-sml/data/F4_FPP_points.txt` and reproduces the expected `1864` “big unitary hash” size for `F4_s`.
- Verifies the set is closed under contragredient (unitary dual) via a C++ helper.

Files:

- C++ wrapper: `atlas-scripts-sml/ffi/atlas_smlffi.cpp`
- Build script: `atlas-scripts-sml/ffi/build.sh`
- SML bindings: `atlas-scripts-sml/ffi/AtlasFFI.sml`
- Script port: `atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml`
- FPP point loader: `atlas-scripts-sml/F4_FPP_points.sml`
- Hash/set: `atlas-scripts-sml/BigUnitaryHash.sml`
- Tests: `atlas-scripts-sml/ffi/test_kgb_size_F4_s.sml`, `atlas-scripts-sml/ffi/test_param_trivial.sml`, `atlas-scripts-sml/ffi/test_param_from_points.sml`

## How it works

Poly/ML can call C functions from shared libraries. Atlas is C++ and exposes C++ APIs, so we introduce a small C ABI “shim” layer:

- Export only `extern "C"` functions with simple argument/return types.
- Hide C++ objects behind opaque `void*` handles (allocated/freed in C++).
- Provide `atlas_last_error()` to return a thread-local error string when C++ throws.

The initial handle type started out as a hardcoded “`F4_s` group”, but it is now generalized to:

- `atlas_group_new_simple(typeLetter, rank, innerClassLetter, realFormNbr)`

## Build & run

Build the shared library:

```sh
atlas-scripts-sml/ffi/build.sh
```

Run the SML test:

```sh
poly -q < atlas-scripts-sml/ffi/test_kgb_size_F4_s.sml
```

Expected output:

```
KGB_size(F4_s) = 229
```

Notes:

- The build uses `-fPIC` and links a `.so`.
- The build uses `-std=gnu++14` because this codebase currently fails to compile cleanly under newer language modes with newer libstdc++ in this environment.
- The build script excludes `sources/io/interactive*.cpp` to avoid dependencies on the readline/input UI layer; the FFI layer should not depend on the interactive front-end.
- The `Param` constructor used by the F4 point loader passes numerator vectors as **text** (space-separated ints) because pointer/array arguments over Poly/ML FFI were not reliable in this environment.

## What still needs to happen (plan)

The `.at` scripts you care about (e.g. `script_to_verify_F4_FPP_unitary_dual.at`) rely heavily on *script-level* infrastructure (e.g. `big_unitary_hash`, `FPP_*` helpers). Most of that is not a single C++ function we can “just call”; it is implemented in the `.at` library itself and must be ported (or re-expressed using C++ equivalents where they exist).

Next steps:

1. **Translate more `.at` library code into SML**
   - Port the minimum subset of `FPP_globalDirac.at`/friends needed beyond the current contragredient check.
2. **Replace remaining script-level primitives**
   - Implement or bind enough operations for `FPP_barycenters`, `FPP_lambdas`, and an `is_unitary` test so we can run the full verifier logic without `.at`.
3. **Stabilise the FFI surface**
   - Keep the C ABI small: prefer “one high-level operation” wrappers (e.g. contragredient) to large low-level APIs, and keep avoiding pointer/array args unless we can make them reliable.

The intent is to keep the C ABI small and stable, and do the bulk of “script logic” in SML.
