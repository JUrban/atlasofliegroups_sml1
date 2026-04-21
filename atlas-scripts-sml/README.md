# Atlas SML Port (Poly/ML)

## What exists so far

This repo currently ships Atlas as a C++ library plus the `atlas` interactive interpreter. The goal here is to run Atlas computations from Standard ML (Poly/ML) so we can translate `atlas-scripts/*.at` into `atlas-scripts-sml/*.sml` without depending on `.at` scripts.

As a first step, there is now a working Poly/ML FFI proof-of-concept that:

- Builds a small C ABI wrapper as a shared library (`.so`) linked against Atlas C++ object code.
- Calls into that shared library from SML using Poly/ML’s `Foreign` interface.
- Constructs Atlas groups from SML (currently “single simple factor” only) and computes `KGB_size(F4_s) = 229`, matching the `atlas` interpreter.

Files:

- C++ wrapper: `atlas-scripts-sml/ffi/atlas_smlffi.cpp`
- Build script: `atlas-scripts-sml/ffi/build.sh`
- SML bindings: `atlas-scripts-sml/ffi/AtlasFFI.sml`
- Test: `atlas-scripts-sml/ffi/test_kgb_size_F4_s.sml`

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

## What still needs to happen (plan)

The `.at` scripts you care about (e.g. `script_to_verify_F4_FPP_unitary_dual.at`) rely heavily on *script-level* infrastructure (e.g. `big_unitary_hash`, `FPP_*` helpers). Most of that is not a single C++ function we can “just call”; it is implemented in the `.at` library itself and must be ported (or re-expressed using C++ equivalents where they exist).

Next steps:

1. **General group construction**
   - Replace the hardcoded `F4_s` constructor with an API that can construct groups by name/type (at least the cases used by the scripts being ported).
2. **Value representation API**
   - Add C ABI constructors/accessors for the core objects the scripts manipulate (e.g. rational vectors, parameters, KGB elements) using opaque handles.
3. **Minimal computational surface for the F4 FPP verifier**
   - Expose enough C++ operations to:
     - enumerate `KGB(G)` (or at least index into it)
     - construct parameters from `(x, lambda, nu)`
     - run whatever “unitarity test” corresponds to `is_unitary` / Dirac machinery in the script library (if there is a C++ equivalent; otherwise port the algorithm).
4. **Port the `.at` library pieces that don’t exist in C++**
   - Re-implement the needed `big_unitary_hash`/FPP workflows in SML (data structures + algorithms), backed by the C++ primitives above.

The intent is to keep the C ABI small and stable, and do the bulk of “script logic” in SML.
