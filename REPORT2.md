# REPORT2: Atlas SML port (interim technical report)

## 1. Background and goal (unchanged)

This repository contains:

- The **Atlas C++ library** implementing root data, real forms, KGB sets, parameters, deformation/finalization, Hermitian forms, (some) unitarity checks, etc.
- The **Atlas interpreter** used to run `.at` scripts in `atlas-scripts/`.

The goal of this project is to **replace the `.at`-script execution layer with Standard ML (Poly/ML)** while continuing to rely on the Atlas C++ implementation for the heavy algebraic objects and computations.

Concretely:

- Important `.at` scripts should gain corresponding `.sml` replacements under `atlas-scripts-sml/`.
- These SML programs should run via `poly` (and be compilable via `polyc` where practical).
- At runtime, the SML programs should **not evaluate any `.at` scripts**.
- The SML code may (and does) call into Atlas C++ via a purpose-built FFI shim layer.

The original “most important” target remains:

- `atlas-scripts/script_to_verify_F4_FPP_unitary_dual.at`

which is now replaced by:

- `atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml`

The core correctness target is to reproduce the same verification checks and final point set size (1864 for `F4_s`) *without* `.at` evaluation.

## 2. Current status (what works today)

### 2.1 The F4 verifier is fully functional and SML-native

The current SML verifier:

- Builds the relevant group (`F4_s`) via FFI.
- Computes the F4 FPP parameter set (expected size **1864**) using SML implementations plus Atlas C++ operations via FFI.
- Runs the verification checks (standard/final, lambda-table consistency, twist-equivalence, hermitian, unitary, unitary-dual closure).

Command:

- `poly -q < atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml`

Expected behavior:

- Prints `true`, runs the checks, and ends by printing `1864`.

### 2.2 All “core fixtures” can be regenerated from SML (no `.at`)

The project still ships some text fixtures under `atlas-scripts-sml/data/` for regression stability, but they can now be regenerated purely from SML:

- `atlas-scripts-sml/data/F4_FPP_barycenters.txt`
- `atlas-scripts-sml/data/F4_FPP_lambdas.txt`
- `atlas-scripts-sml/data/F4_FPP_points.txt`

Generators:

- `atlas-scripts-sml/data/gen_F4_FPP_aux_data_sml.sh` (barycenters + lambdas)
- `atlas-scripts-sml/data/gen_F4_FPP_points_sml.sh` (points)

Policy:

- These generators write `*.new` by default to avoid accidental noisy diffs.
- To overwrite the tracked fixtures, run with `--overwrite` (or `OVERWRITE=1`).

### 2.3 Additional `.at` ports and “library” growth

Beyond the F4 verifier, the SML side now includes ports/adapters for several other `.at` building blocks:

- Spherical unitary point tables (`unitary.at` → `atlas-scripts-sml/unitary.sml`) for `F4`, `D4`, `E7`, with careful coordinate conversion where required.
- A partial `test_unitarity.at` harness (`atlas-scripts-sml/test_unitarity.sml`) + runnable driver (`atlas-scripts-sml/test_unitarity_main.sml`).
- A growing subset of `representations.at` constructors (`atlas-scripts-sml/representations.sml`), including:
  - minimal principal series,
  - finite dimensional constructors,
  - large fundamental/discrete series (with a conservative equal-rank test),
  - discrete series (via `from_dominant` witness + KGB cross),
  - Harish–Chandra parameter extraction (`hc_parameter`) via `cross_divide`.
- A port of `cross_W_orbit.at` essentials (`atlas-scripts-sml/cross_W_orbit.sml`).
- A block wrapper (`ParamBlocks`) exposing “block survivors” via FFI (enabling `trivial_block`, etc.).

## 3. High-level architecture

### 3.1 The FFI design

Atlas is C++, while Poly/ML’s FFI calls C ABIs. The project therefore uses a small bridging layer:

- C++ shim: `atlas-scripts-sml/ffi/atlas_smlffi.cpp`
  - exports `extern "C"` functions with simple argument types
  - hides C++ objects behind opaque pointers
  - catches exceptions and reports errors via `atlas_last_error()`
- SML bindings: `atlas-scripts-sml/ffi/AtlasFFI.sml`
  - binds those symbols using Poly/ML `Foreign`
  - provides minimal SML-level types and wrappers

Design choices (important for correctness and maintainability):

- **Text-based interchange** for many structured values (weights, matrices, words).
  - In this environment, pointer/array arguments across Poly/ML FFI were brittle.
  - Passing “`int list text`” is robust and debuggable, and it matches the Atlas interpreter’s own text conventions.
- **Small but growing surface**.
  - Prefer high-value, semantically meaningful entry points (e.g. “construct parameter from lambda/nu text”, “compute hermitian form irreducible”) rather than attempting to expose all internal C++ classes.
- **Explicit ownership**.
  - The SML code treats every `group`, `param`, `ktypepol`, etc. as an owned external resource.
  - Modules document when they allocate and when they free.

### 3.2 The SML layer structure (conceptual)

The `atlas-scripts-sml/` tree has converged into a few layers:

1. **FFI bindings** (`atlas-scripts-sml/ffi/AtlasFFI.sml`)
2. **Low-level data types / algorithms**
   - lattice arithmetic: `Lattice.sml`, `IntMatrix.sml`, `MatrixAT.sml`, `lattice_aux.sml`
   - sorting/combinatorics: `basic.sml`, `sort.sml`, `combinatorics.sml`
3. **Atlas-object adapters**
   - parameter sets: `ParamHash.sml`, `BigUnitaryHash.sml`, `ParamFinals.sml`, `ParamBlocks.sml`
   - Hermitian/K-type utilities: `Hermitian.sml`, `KTypePol.sml`, `KType.sml`, `LowestKTypes.sml`
   - Weyl-word / dominance utilities: `WeylWord.sml`, `FromDominant.sml`, `Dominant.sml`, `cross_W_orbit.sml`
4. **Translated script logic**
   - FPP geometry and enumeration: `FPP_vertices_fold.sml`, `FPP_barycenters_fold.sml`, `FPP_lambdas_fold.sml`, etc.
   - the F4 verifier: `VerifyF4FPP.sml` + entry script
   - unitarity harness: `test_unitarity.sml` + `test_unitarity_main.sml`
5. **Data regeneration scripts**
   - `atlas-scripts-sml/data/gen_*.sml` plus shell wrappers

This is intentionally “library-like”: as more `.at` scripts are ported, they should mostly be able to reuse these layers rather than growing one-off logic per script.

## 4. What the key SML scripts do (and how they relate)

This section is a high-level tour of the “most important” `.sml` scripts/modules, grouped by workflows.

### 4.1 The F4 FPP unitary-dual verification workflow

**Entry point**

- `atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml`
  - Minimal runner; loads `VerifyF4FPP` and runs `VerifyF4FPP.run()`.

**Driver**

- `atlas-scripts-sml/VerifyF4FPP.sml`
  - Configures the FPP flags (mirroring the `.at` verifier’s global variables).
  - Computes the FPP parameter hash for `F4_s` using:
    - `atlas-scripts-sml/F4_FPP_points_compute.sml`
  - Runs the verification checks using:
    - `atlas-scripts-sml/FPP_globalDirac.sml`

**Point computation**

- `atlas-scripts-sml/F4_FPP_points_compute.sml`
  - Implements the “compute parameters from folded barycenters + lambda table” scan:
    - enumerate KGB elements `x`
    - enumerate candidate lambdas for `x`
    - enumerate compatible barycenters `gamma`
    - form `nu` and construct/normalize parameters
    - filter to standard/final/hermitian and (when enabled) unitary
    - insert into `ParamHash` (deduplicated by parameter equality)

**FPP combinatorics inputs**

- `atlas-scripts-sml/FPP_vertices_fold.sml`
  - Computes the folded vertex set used for the FPP geometry (group-specific).
- `atlas-scripts-sml/FPP_barycenters_fold.sml`
  - Computes barycenters (rational points) of the relevant folded faces.
- `atlas-scripts-sml/FPP_lambdas_fold.sml`
  - Computes the lambda families used in the FPP parameter construction.

**Verification checks**

- `atlas-scripts-sml/FPP_globalDirac.sml`
  - Implements the “checker” side of the pipeline:
    - standard/final sanity checks
    - lambda-table consistency
    - twist-equivalence closure
    - hermitian check
    - unitary check (using Atlas’ built-in unitary predicate)
    - unitary-dual closure under contragredient

### 4.2 Spherical unitary tables and the unitarity harness

**Data tables**

- `atlas-scripts-sml/unitary.sml`
  - Port of `atlas-scripts/unitary.at` point tables for `F4`, `D4`, `E7`.
  - Includes a key coordinate-system correction:
    - `D4` points in the `.at` table are in standard coordinates,
      while `atlas_group_new_simple` uses fundamental-weight coordinates.
    - The SML port includes an explicit conversion for `D4`.

**Harness**

- `atlas-scripts-sml/test_unitarity.sml`
  - Partial port of `atlas-scripts/test_unitarity.at`.
  - Builds parameters (often minimal spherical principal series) and checks unitarity via Hermitian-form purity.
  - Designed to stream parameters (construct → test → free) to reduce memory overhead.

**Runner**

- `atlas-scripts-sml/test_unitarity_main.sml`
  - Simple CLI-like driver for checking prefixes of the `F4/D4/E7` tables.
  - Supports `verbose` and `heavy` gating for larger runs.

### 4.3 Representation constructors and orbit helpers

**Common constructors**

- `atlas-scripts-sml/representations.sml`
  - Partial port of `atlas-scripts/representations.at`.
  - Provides a growing set of constructors and helpers used by other translated scripts:
    - minimal principal series (split groups)
    - finite dimensional parameters
    - large fundamental/discrete series (with equal-rank detection)
    - discrete series from a Harish–Chandra parameter:
      - uses `FromDominant` to get a dominance witness word
      - uses `WeylWord` cross action to transport KGB elements
    - `hc_parameter` via `cross_divide` and Weyl-word action on weights
    - `trivial`, `trivial_block`, `block_of`

**Dominance and Weyl words**

- `atlas-scripts-sml/FromDominant.sml`
  - Wrapper for the Atlas operation `RootDatum::factor_dominant`:
    returns a witness word and the dominant representative.
- `atlas-scripts-sml/WeylWord.sml`
  - Minimal Weyl-word type (`int list`) with:
    - serialization to text for FFI
    - inversion (word reversal)
    - KGB cross action using the `.at` left-cross convention
    - action on rational weights via a C++ shim helper

**Cross orbit search**

- `atlas-scripts-sml/cross_W_orbit.sml`
  - BFS-based port of the key `cross_W_orbit.at` routines:
    - `cross_divide`
    - `cross_orbit`
    - `is_in_cross_orbit`

**Block wrappers**

- `atlas-scripts-sml/ParamBlocks.sml`
  - Wrapper for computing block survivor lists (plus start index) via FFI.

## 5. Build/run guide (current “known-good” commands)

### 5.1 Build the FFI shared library

- `bash atlas-scripts-sml/ffi/build.sh`

### 5.2 Run the F4 verifier

- `poly -q < atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml`

### 5.3 Run unitarity table checks

- `poly -q < atlas-scripts-sml/test_unitarity_main.sml`
- `poly -q < atlas-scripts-sml/test_unitarity_main.sml -- D4 10`
- `poly -q < atlas-scripts-sml/test_unitarity_main.sml -- E7 5 verbose`

### 5.4 Regenerate fixtures from SML

- `atlas-scripts-sml/data/gen_F4_FPP_aux_data_sml.sh`
- `atlas-scripts-sml/data/gen_F4_FPP_points_sml.sh`

Use `--overwrite` to replace the tracked fixtures.

## 6. Validation strategy and test inventory

The development style remains incremental:

- Add one new C++ shim function (or one new SML module).
- Add a small Poly/ML test under `atlas-scripts-sml/ffi/`.
- Only then rely on the new functionality in a workflow script.

The `atlas-scripts-sml/ffi/` folder contains a growing collection of smoke/regression tests, including:

- Hermitian-form purity vs built-in unitary checks (agreement tests).
- F4 points computed vs fixture tables.
- `from_dominant`, Weyl-word action, cross orbit (`cross_divide`) tests.
- Block survivors (`block(p)`) tests.

The highest-signal integration regression remains:

- `poly -q < atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml`

## 7. Technical problems encountered (and current resolutions)

### 7.1 “Text as ABI” and negative numbers

Poly/ML prints negative integers as `~n`, while Atlas parsers expect `-n`. Many modules provide small helpers to convert between these conventions when producing text.

### 7.2 FFI representation pitfalls (example: KTypePol term counting)

A notable real bug fixed in the process:

- The C++ `KTypePol` representation (`Free_Abelian_light`) stores an internal array whose `.size()` is only an upper bound (including zero coefficients).
- Correct iteration must use `.count_terms()` instead.

The shim now uses the correct notion, preventing crashes in heavier runs.

### 7.3 Coordinate system mismatches (example: `D4` spherical unitary table)

Some `.at` tables implicitly assume particular coordinate systems (standard vs fundamental weight coordinates). The SML port now makes these conversions explicit, and tests are written against the resulting semantics.

## 8. Outlook / next steps

The project is now in a “positive flywheel” state:

- We have enough infrastructure to port more `.at` scripts in a mostly mechanical way:
  - identify the `.at` operations used,
  - add small targeted FFI helpers when necessary,
  - re-express the orchestration in SML with explicit ownership and tests.

Likely near-term targets:

1. Extend `representations.sml` with additional constructors used pervasively in `.at` scripts (as needed by the next scripts you care about).
2. Port additional verification scripts (beyond F4) to build confidence that the approach scales.
3. Continue consolidating a “stable” SML-facing Atlas API (still intentionally small).

## Appendix A: Glossary (project-local)

- **`.at`**: Atlas scripting language files executed by the Atlas interpreter (`atlas` binary).
- **FFI**: Poly/ML Foreign Function Interface to the Atlas C++ code, via `libatlas_smlffi.so`.
- **KGB element `x`**: Index of a KGB element in a real form; determines an involution `θ`.
- **`θ` / theta**: Involution matrix attached to a KGB element.
- **`λ` / lambda**: Integral weight input used in the FPP construction.
- **`γ` / gamma**: Rational barycenter point from folded FPP geometry.
- **`ν` / nu**: Rational shift computed from `(λ,γ,θ)` used to build an Atlas parameter.
- **Standard/final/hermitian/unitary**: Atlas parameter predicates exposed via FFI.

