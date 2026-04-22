# REPORT: Replacing Atlas `.at` scripts with Standard ML

## 1. Background and goal

This repo contains the Atlas C++ library (root data, Weyl groups, parameter objects, unitary tests, etc.) and an Atlas scripting interpreter that runs `.at` scripts. The goal of this effort is to **replace the `.at`-script execution layer with Standard ML (SML)** while still relying on the underlying C++ implementation for the heavy algebraic objects and computations.

Concretely:

- Each important `.at` script in `atlas-scripts/` should be replaced by a corresponding `.sml` program in `atlas-scripts-sml/`.
- The SML program should be runnable via `poly` (and eventually buildable via `polyc`) and should **not require running any `.at` script at runtime**.
- The SML code may depend on an SML-facing API/binding layer that calls into the Atlas C++ library (FFI).
- The primary “most important” target is the Atlas script:
  - `atlas-scripts/script_to_verify_F4_FPP_unitary_dual.at`
  - This script verifies the F4 “FPP unitary dual” computation, using a specific finite set of “bottom-layer/facet” points (historically 1864 points for the relevant F4 real form).

The key correctness target is that the SML version should be able to reproduce (and then verify) the same data and checks as the `.at` version, at least for this F4 pipeline.

## 2. Current status (what works today)

The SML port has reached a stable checkpoint:

- `atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml` runs successfully under Poly/ML and performs the verification checks.
- It still loads the known 1864-point set from a data file:
  - `atlas-scripts-sml/data/F4_FPP_points.txt`
- A command that currently works:
  - `poly -q < atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml`
  - It prints `true`, runs the checks, and ends by printing `1864`.

In parallel, the SML code can now generate a **superset** of the 1864-point set from scratch (no `.at` dependency, no point-file dependency):

- `atlas-scripts-sml/F4_FPP_points_compute.sml` computes a set of “unitary barycenter parameters”.
- For the relevant F4 real form, that computed set currently has size **3758**, and the official 1864-point set is a **strict subset** of it.
- A test to confirm the subset relation exists:
  - `poly -q < atlas-scripts-sml/ffi/test_F4_points_compute_match_file.sml`
  - This loads the 1864 file-based points, computes the 3758 set, and verifies that every file point occurs in the computed set.

So the situation is:

1. The verifier and support modules are usable and compile.
2. The full generator pipeline is not yet exact: it over-generates and is missing the final “bottom-layer selection” step that reduces 3758 down to exactly 1864.

## 3. Repository layout for the SML work

The SML work lives in `atlas-scripts-sml/`:

- **Entry points**
  - `atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml`
  - `atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual_main.sml`
  - `atlas-scripts-sml/VerifyF4FPP.sml`
- **FFI layer**
  - `atlas-scripts-sml/ffi/AtlasFFI.sml` (SML declarations and wrappers)
  - `atlas-scripts-sml/ffi/atlas_smlffi.cpp` (C++ entry points exposed to SML)
  - `atlas-scripts-sml/ffi/libatlas_smlffi.so` (built shared library)
  - `atlas-scripts-sml/ffi/build.sh` (build helper)
- **Geometric / combinatorial reconstruction (SML-side)**
  - `atlas-scripts-sml/FPP_vertices_fold.sml` (vertices of folded fundamental region; e.g. F4 count 231)
  - `atlas-scripts-sml/FPP_barycenters_fold.sml` (barycenters; e.g. F4 count 9789)
  - `atlas-scripts-sml/FPP_faces_geom_fold.sml`, `atlas-scripts-sml/FPP_fundamental_alcove.sml`, `atlas-scripts-sml/cofolded.sml`
  - Supporting modules: `Coordinates.sml`, `Lattice.sml`, `MatrixAT.sml`, `IntMatrix.sml`, etc.
- **Parameter logic / unitary tests**
  - `atlas-scripts-sml/RootDatum.sml` (root data wrapper, including C++-backed functions)
  - `atlas-scripts-sml/AllParameters.sml` (constructing/normalising parameters, etc.)
  - `atlas-scripts-sml/ParamHash.sml`, `atlas-scripts-sml/ParamFinals.sml`, `atlas-scripts-sml/ParamReduce.sml`
  - `atlas-scripts-sml/FPP_globalDirac.sml` (verification checks; “checks-only”, not a full generator)
- **F4-specific**
  - `atlas-scripts-sml/FPP_lambdas_fold.sml` (computes the needed lambda sets from scratch)
  - `atlas-scripts-sml/F4_FPP_points_compute.sml` (current generator superset)
  - `atlas-scripts-sml/data/F4_FPP_points.txt` (legacy exact point set; currently used by verifier)

There is also a large collection of small SML tests in `atlas-scripts-sml/ffi/` that were used to validate pieces of the binding layer and the SML-side combinatorics.

## 4. Core approach: “port algorithms, not scripts”

Atlas `.at` scripts do two distinct things:

1. Orchestrate computation by calling into C++ library functions (root data construction, KGB data, parameter normalisation, unitary checks, etc.).
2. Implement “glue logic” and some nontrivial combinatorics/geometry in the scripting layer (often using `hash` tables, loops, and helper `.at` scripts).

Replacing the interpreter with SML can be done in two complementary ways:

- **Binding approach (preferred for complex primitives):** expose the relevant C++ functions directly to SML, and replicate `.at` control flow in SML.
- **Reimplementation approach (preferred for pure combinatorics):** implement the needed geometric/combinatorial algorithms in SML to avoid excessive FFI chatter.

This effort uses both:

- C++ remains responsible for “semantic” Atlas objects (root data, Weyl group actions, KGB involutions, parameter normalisation, unitarity testing).
- SML reconstructs the specific FPP-folded geometry (vertices/barycenters of the fundamental region) and does the iteration/filtering logic.

The intent is to end with an SML program that is “morally the same script”, but written in a typed language with explicit modules, and with the computationally heavy pieces delegated to the existing C++.

## 5. The F4 FPP pipeline (what is being computed)

The target script `atlas-scripts/script_to_verify_F4_FPP_unitary_dual.at` is part of a workflow that can be summarized as:

1. Fix a real form of a complex group (here: an F4 real form, often referred to as “F4_s” in local naming).
2. Enumerate certain families of parameters arising from:
   - KGB elements `x` (or data tied to them),
   - lattice elements `lambda` associated to FPP orbits,
   - and certain “face/barycenter points” `gamma` in a folded fundamental domain.
3. For each triple `(x, lambda, gamma)` satisfying linear compatibility constraints (coming from an involution `theta = involution(x)`), construct an Atlas parameter, normalise it, and test whether it is:
   - standard / final / hermitian,
   - and unitary in the required sense (for this pipeline, “c-form” unitarity checks are used).
4. The `.at` pipeline uses a special finite subset of the barycenter-derived points called the “bottom layer” / “unitary facet points” (size 1864 for the target case). This is crucial for performance and for matching the expected verification results.

At a high level, the SML port has already succeeded in steps (1)–(3), and the remaining missing piece is step (4): reproducing exactly the same “bottom-layer selection” as the `.at` workflow.

## 6. FFI design and philosophy

The binding layer lives in:

- `atlas-scripts-sml/ffi/atlas_smlffi.cpp`
- `atlas-scripts-sml/ffi/AtlasFFI.sml`

The guiding principles used so far:

- Expose C++ functions at a level that is stable and testable from SML (small functions with string/byte-oriented interfaces where convenient).
- Keep memory ownership clear. For complex Atlas objects, SML typically holds an opaque “handle” (pointer-sized value) and calls C++ to operate on it.
- Prefer returning compact serializations (e.g. a text representation of a parameter) when that avoids exposing a large object graph through the FFI.
- Add focused tests under `atlas-scripts-sml/ffi/` for each new binding.

One concrete example already in use:

- `RootDatum.FPP_orbit_numers` is a binding to a C++ routine (internally `weyl::FPP_orbit_numers`) needed for the FPP lambda generation logic.

The overall goal is to build enough “SML-facing Atlas API” to express the `.at` script workflows naturally in SML while keeping the binding surface minimal.

## 7. The current SML computation for F4: barycenters and lambdas

Two large components have already been reconstructed purely in SML (with limited C++ help for semantic data):

### 7.1 Folded vertices and barycenters

- `atlas-scripts-sml/FPP_vertices_fold.sml` computes the relevant vertex set in the folded fundamental region.
  - For F4 in this setup, the vertex count is 231.
- `atlas-scripts-sml/FPP_barycenters_fold.sml` computes the set of barycenters (candidates for `gamma`).
  - For F4 in this setup, the barycenter count is 9789.

These are used as a finite “sampling set” on which the FPP geometry/unitarity computations are done.

### 7.2 FPP lambda generation

The lambda computation has been made “from scratch” and no longer depends on a precomputed file:

- `atlas-scripts-sml/FPP_lambdas_fold.sml` computes the lambda families needed for the F4 pipeline.
- This matches the previous data stored in `atlas-scripts-sml/data/F4_FPP_lambdas.txt`, so the verifier no longer needs to load that file.

This is an important milestone because it proves that the SML side can reconstruct nontrivial inputs to the unitary enumeration that were previously treated as fixed data.

## 8. Generating unitary barycenter parameters in SML (the 3758 superset)

The current generator is implemented in:

- `atlas-scripts-sml/F4_FPP_points_compute.sml`

It follows the (now clarified) parameter-construction semantics used in the `.at` scripts:

- Let `x` be a KGB element, and set `theta = involution(x)`.
- Define the “plus” operator `onePlus = I + theta` on the relevant lattice.
- Given a lattice element `lambda`, compute `thetaPlus = (I + theta) * lambda`.
- Let `gamma` be a barycenter candidate. The compatibility condition is:
  - `(I + theta) * gamma = (I + theta) * lambda`
- When that holds, define:
  - `nu = gamma - ((I + theta) * lambda) / 2`
- Construct the corresponding Atlas parameter from `(lambda, nu)` (and the fixed `x`-data), then:
  - normalise it,
  - filter by `standard`, `final`, `hermitian`,
  - and (optionally depending on flags) test `unitary` in the c-form sense.

This generator enumerates:

- all `(x, lambda)` families from `FPP_lambdas_fold.FPP_lambdas_table g`, and
- all barycenters `gamma` from `FPP_barycenters_fold.barycenters_all g`.

It then filters to those that satisfy the compatibility equation and pass the unitary-related predicates.

For the target F4 real form, the resulting set size is:

- `|computedAll| = 3758`

and we have a regression test showing:

- `F4_FPP_points.txt` (1864 points) ⊆ `computedAll` (3758 points).

This is a major step because it demonstrates that:

- The SML port can reconstruct and compute a large amount of the intended point set without relying on `.at`.
- The remaining discrepancy is due to a specific selection/refinement step, not due to missing basic functionality.

## 9. The remaining gap: reproducing the “bottom layer” exactly

The central unsolved problem is:

> Identify and implement the selection criterion that reduces the 3758 computed unitary barycenter parameters to the exact 1864 “bottom-layer/facet” parameters used by the `.at` verification pipeline.

Important facts established so far:

- The 1864 set is not “all unitary barycenter parameters”; it is a strict subset.
- The over-generation is global: it is not merely “extra interior points” within the same `(x, lambda)` family.
  - Entire `(x, lambda)` families exist in the 3758 set that do not occur at all in the official 1864 set.
- Simple heuristics do not reproduce the official selection:
  - a naive “boundary of union of maximal simplices” idea per family did not match,
  - simple “height cutoffs” do not match,
  - twist-canonicalization did not shrink the computed set.

This indicates that the `.at` pipeline’s “bottom layer” selection is implementing a more subtle condition, likely tied to:

- a geometric minimization principle (global across families),
- a “Dirac inequality / Dirac cohomology” constraint that is not being applied in the current generator,
- or a particular definition of “facet points” that is not simply “all barycenters satisfying (I+θ)γ=(I+θ)λ”.

In the `.at` sources, the relevant logic appears to be spread across multiple scripts, including:

- `atlas-scripts/bottom_layer.at`
- `atlas-scripts/FPP_localDirac.at`
- `atlas-scripts/FPP_globalDirac.at`
- `atlas-scripts/FPP_faces_herm.at`

The SML side currently contains:

- `atlas-scripts-sml/FPP_globalDirac.sml` as a verification/checking module,

but it does not yet reproduce the `.at`-side bottom-layer construction.

## 9.1 Data representation details (what “gamma”, “lambda”, “theta” actually are in code)

One recurring source of confusion when translating `.at` scripts is that the script layer often blurs multiple coordinate systems and multiple lattices (weight lattice, cocharacter lattice, restricted lattices, folded coordinates, etc.). The SML implementation makes these explicit through a small family of types and conversion functions.

The current design (as it exists in `atlas-scripts-sml/`) can be summarized as:

- **Integers and rationals**
  - `Rat.sml` implements rational numbers (used for barycenter coordinates).
- **Integer vectors and matrices**
  - `IntMatrix.sml` and `MatrixAT.sml` provide the matrix operations needed for lattice maps such as `(I+θ)` and for Smith/diagonal forms used in quotient computations.
- **Coordinates / lattices**
  - `Coordinates.sml`, `Lattice.sml`, `lattice_aux.sml`, and `LatticeAT.sml` provide:
    - “ambient” free abelian groups as `Z^n`,
    - sublattices given by basis matrices,
    - operations like image, preimage, intersection, saturation, quotient bases,
    - and affine slicing (layers) used in the FPP face construction.

At the “FPP computation level”:

- `lambda` is represented as an integral vector in the relevant lattice basis.
- `gamma` is represented as a rational vector (barycenter coordinate), typically with denominators coming from simplex barycenter fractions.
- `theta` is represented as an integral matrix acting on the relevant lattice.
- `onePlus` is represented as the integral matrix `I + theta`.

This makes the core compatibility equation and parameter-construction steps easy to state and debug:

- Check: `onePlus * gamma == onePlus * lambda`
- Compute: `nu = gamma - (onePlus * lambda)/2`

## 9.2 Parameter hashing and canonicalization

The `.at` scripts frequently store and compare parameters using “hash keys” derived from their textual prints. The SML port follows a similar approach for reliability and cross-language comparability:

- Many modules use a canonical “parameter text” representation as the stable identity of a parameter.
- `ParamHash.sml` and related modules provide:
  - insertion into hash tables keyed by parameter text,
  - size and membership checks,
  - stable iteration ordering where needed for reproducibility.

This has two practical benefits:

1. It makes “computed vs file” comparisons straightforward (they are comparisons of canonical texts).
2. It avoids subtle pointer-identity bugs across the FFI boundary.

The generator in `F4_FPP_points_compute.sml` uses “insert into a hash keyed by param text” as the primary deduplication mechanism.

## 10. Technical options to solve the bottom-layer selection

There are two plausible implementation strategies.

### Option A: Port the `.at` bottom-layer algorithm to SML

This would mean:

- Read `bottom_layer.at` and the relevant helpers.
- Identify the core data structures used (hashing by parameter, grouping by orbit, selection by face/facet, etc.).
- Re-implement those steps in SML, using the existing SML geometry modules (vertices/barycenters) and existing FFI calls (parameter normalisation/unitarity).

Pros:

- Keeps logic transparent and directly comparable to `.at`.
- Avoids increasing the C++ surface area.

Cons:

- The `.at` scripts may use interpreter-specific conveniences (implicit coercions, hash semantics, ad hoc canonicalizations).
- Reproducing exact semantics could take time and repeated testing against known outputs.

### Option B: Add a C++ helper that exposes “bottom-layer points” directly

This would mean:

- Identify the C++ routines used indirectly by `.at` for bottom-layer selection (if they exist), or implement the bottom-layer selection once in C++.
- Expose a single function like “compute bottom-layer params for given group and flags” to SML.

Pros:

- Likely to be fastest and closest to existing Atlas internal notions if those are already in C++.
- Reduces SML-side complexity for the most delicate matching step.

Cons:

- Risks embedding “policy” into C++ without a clear audit trail.
- Might be harder to iterate if the correct algorithm requires frequent modification.

A hybrid approach is also reasonable:

- implement the selection in SML first for clarity,
- and later migrate a stable version into C++ if performance becomes critical.

## 11. Performance considerations

The current `computeAll` in `F4_FPP_points_compute.sml` is intentionally straightforward:

- It loops over all `(x, lambda)` families and all barycenters `gamma`.
- It checks the compatibility equation and then constructs and tests parameters.

This is computationally heavy but still acceptable for development (minutes, not hours), and it has been extremely useful for correctness debugging because it establishes a “superset envelope” that must contain the official set.

Once the bottom-layer selection is implemented, performance should improve because the final selection should drastically reduce the number of parameter constructions and unitary tests.

Potential optimizations (if needed later) include:

- pre-index barycenters by their `(I+θ)γ` image to avoid scanning all 9789 for every `(x,λ)`,
- cache normalizations and unitary decisions by parameter text,
- reduce FFI call count by batching checks in C++.

## 12. Validation strategy and current regression checks

The approach to validation so far has been incremental:

- Add a binding or SML module.
- Create a small test in `atlas-scripts-sml/ffi/` that asserts a concrete expected result (counts, subset relations, sanity checks).
- Only then use it in the main F4 verifier pipeline.

Important existing regression signals include:

- F4 barycenter count matches expected (9789).
- F4 vertex count matches expected (231).
- Lambda table generation matches the legacy file.
- File-points ⊆ computedAll superset (1864 ⊆ 3758).
- The verifier pipeline still succeeds on the file points.

The final regression goal is:

- computedBottomLayer == filePoints (exact match of 1864 points),

after which the file can be demoted to a test fixture (or removed entirely, depending on preference).

## 12.1 “What broke / what we learned” (translation lessons)

A few important lessons emerged during the port:

- **Be explicit about the parameter construction formula.** An early incorrect attempt used a different expression for `nu` (e.g. involving `(I-θ)`), which produced plausible-looking points but did not match the `.at` semantics. The current formula `nu = gamma - (I+θ)λ/2` is the correct one for the barycenter-based construction used in the FPP pipeline.
- **Do not assume “all compatible barycenters” are “facet points”.** The existence of 3758 computed unitary barycenter parameters vs 1864 official facet points shows that the `.at` pipeline performs a nontrivial selection, not merely a compatibility + unitary filter.
- **Reconstructing the geometry in SML is feasible and robust.** The folded vertex and barycenter counts matching expectations is a strong signal that the SML combinatorics are aligned with the intended geometry.
- **FFI boundaries are best kept narrow.** The current binding approach (opaque handles + string/text interfaces) makes it easier to debug and to compare SML vs `.at` results.

## 13. Near-term plan (next milestones)

The next steps are tightly focused on solving the 3758 → 1864 selection step.

### Milestone 1: Identify the selection semantics precisely

- Read and isolate the exact selection logic in the `.at` scripts (`bottom_layer.at` and friends).
- Add small probes (either via SML instrumentation or direct Atlas runs) to extract intermediate sizes and invariants from the `.at` pipeline for the same group.
- Define the target selection function in a way that is testable (e.g. “given computedAll, produce exactBottomLayer”).

Deliverable:

- A documented set of invariants and intermediate counts that are stable across both implementations.

### Milestone 2: Implement the selection in SML (first correct, then fast)

- Implement a first-pass selection in SML that matches the `.at` result exactly.
- Add a regression test asserting exact equality with `data/F4_FPP_points.txt`.
- Only after correctness is established, optimize the implementation.

Deliverable:

- `F4_FPP_points_compute.sml` (or a new module) gains `computeBottomLayer` producing exactly 1864 points.

### Milestone 3: Remove runtime dependency on `F4_FPP_points.txt`

- Update `VerifyF4FPP.sml` (and the entry script) to use the computed bottom-layer points rather than loading the file.
- Keep `F4_FPP_points.txt` only as a regression fixture (optional).

Deliverable:

- `atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml` runs “from scratch” and ends with `1864` without reading the points file.

### Milestone 4: Generalize beyond F4

- Once the bottom-layer logic is solid, the remaining `.at` scripts can be translated in a more mechanical way:
  - port script orchestration to SML,
  - reuse the same binding and geometry modules,
  - add tests for each new script.

Deliverable:

- Additional `.at` scripts replaced by `.sml` scripts under `atlas-scripts-sml/`.

## 14. Risks, open questions, and outlook

The main risk is that the `.at` “bottom-layer” definition is subtle and may depend on:

- specific ordering conventions in hash keys or normal forms,
- implicit canonicalization steps,
- or script-level filtering that is not obviously tied to a single mathematical property.

However, the outlook is good because:

- the SML port already reconstructs all of the raw ingredients (vertices, barycenters, lambdas, involutions, parameter construction),
- and the official point set is confirmed to be contained in a computable, from-scratch superset.

This means the remaining work is not “missing primitives” but “matching one selection algorithm”.

Once solved for F4, it should be reusable across other scripts and groups, and SML will effectively become a typed, maintainable replacement for the `.at` interpreter for this class of workflows.

## 14.1 Why the “3758 vs 1864” mismatch is not a small bug

Several diagnostics suggest that the mismatch is not due to a minor normalization issue:

- The file-based set is fully contained in the computed superset; i.e. the generator is not “missing” the official points.
- The extra points are not confined to “interior points of the same families”; they introduce entirely new `(x,lambda)` families.
- Simple global filters (e.g. height cutoffs) do not isolate the 1864 points.

This points strongly toward the conclusion that “bottom layer / unitary facet points” is a distinct concept from “all unitary-compatible barycenter parameters”.

## 15. Build and integration outlook (Poly/ML now, `polyc` later)

The current workflow runs the scripts via the Poly/ML REPL (`poly`) and uses the dynamically loaded shared library `libatlas_smlffi.so`.

To move toward a more production-like setup, there are a few foreseeable steps:

- Ensure `atlas-scripts-sml/` can be built into a standalone executable via `polyc` (or via a small wrapper that preloads the FFI `.so`).
- Provide a consistent “main module” entry point per translated script (many scripts already have `*_main.sml` stubs for this purpose).
- Potentially package the SML modules as a small “atlas-sml” library layer that the script replacements depend on.

This can be done after the bottom-layer selection is correct; correctness is the current priority.

## 15. How to run key checks today

- Run the current verifier (still file-based points):
  - `poly -q < atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml`
- Run the computedAll superset vs file subset test:
  - `poly -q < atlas-scripts-sml/ffi/test_F4_points_compute_match_file.sml`
- (Optional) rebuild the FFI shared library if needed:
  - `bash atlas-scripts-sml/ffi/build.sh`

## Appendix A: Glossary (project-local)

- **`.at`**: Atlas scripting language files executed by the Atlas interpreter (`atlas` binary).
- **FFI**: Foreign Function Interface between Poly/ML and the Atlas C++ code.
- **KGB element `x`**: Atlas object encoding a (real form dependent) involution/conjugacy datum; used to derive `θ = involution(x)`.
- **`θ` / “theta”**: an involution acting on a lattice (represented as an integer matrix in SML).
- **`λ` / “lambda”**: an integral lattice element used in the FPP orbit/lambda enumeration.
- **`γ` / “gamma”**: a rational barycenter point (from folded simplex barycenters).
- **`ν` / “nu”**: a rational shift derived from `(λ, γ, θ)` used to build an Atlas parameter.
- **“bottom layer” / “facet points”**: the special finite subset (1864 points in the target case) used by the `.at` pipeline for verification; not yet reconstructed exactly by SML.

