# `cheat` inventory (HOL4)

This file tracks where the current HOL4 development is intentionally
`CHEAT`-tainted and what each `cheat` represents in the overall plan from
`VERIFY_ESTIMATE.md`.

The goal is not to eliminate all `cheat`s immediately, but to:

1. keep the *logical structure* of the end-to-end argument explicit,
2. ensure each `cheat` corresponds to a well-scoped future proof obligation,
3. make it clear which parts are “pure logic / list reasoning” (already OK)
   versus which parts need CakeML proofs and/or Atlas/FFI contracts.

## A. Atlas/FFI contract assumptions

- File: `formal/hol4/F4FPPVerifyAtlasFFIContractsGoalsScript.sml`
- Role: records the (currently axiomatic) laws expected of Atlas equality/hash
  and related primitives.
- Note: the bottom-layer congruence contract `atlas_eq_congruent_bottom_layer`
  now also covers `lambda_table_ok` and congruence of `param_equiv` in both
  arguments (needed for modulo-`atlas_eq` bottom-layer reasoning).
- Main placeholder:
  - `atlas_ffi_contracts_hold` : assumes `atlas_ffi_contracts`.

Planned discharge options:
- keep axiomatic (explicit trusted base),
- validate empirically (testing harness),
- or partially justify by a verified FFI model + CakeML extraction for the
  pure parts.

## B. “Program success” bridge (Poly/ML control-flow)

These `cheat`s isolate the connection between the actual Poly/ML programs in
`atlas-scripts-sml/` and the abstract HOL obligations.

- File: `formal/hol4/F4FPPVerifySMLBridgeGoalsScript.sml`
- Role: `fast_program_succeeds` / `slow_program_succeeds` are abstract
  predicates; success should imply the abstract obligations.
- Status: all program-success implications are placeholders (`cheat`).

Planned discharge:
- refactor SML to be I/O-free and return structured results,
- then prove (in CakeML or a shallow HOL model) that successful evaluation
  implies the stated obligations.

## C. Refined-main route bridges (program success ⇒ refined obligations)

- File: `formal/hol4/F4FPPVerifyRefinedBridgeGoalsScript.sml`
- Placeholders:
  - `fast_program_succeeds_imp_refined_fast_obligations`
  - `fast_program_succeeds_imp_refined_fast_obligations_atlas_eq`
  - `slow_program_succeeds_imp_refined_slow_obligations`
  - `slow_program_succeeds_imp_refined_slow_obligations_atlas_eq`

Role:
- connects concrete success predicates to the obligation bundles used by the
  refined-main theorems, including the modulo-`atlas_eq` variant.

## D. Fast program phase split

- File: `formal/hol4/F4FPPVerifyFastProgramSplitBridgeGoalsScript.sml`
- Placeholder:
  - `fast_program_succeeds_imp_phase_success`

Role:
- makes the fast program’s control-flow (“compute phase; then bottom-layer
  checks”) explicit in the formal decomposition.

## E. Fast compute phase ⇒ obligations

- Files:
  - `formal/hol4/F4FPPVerifyFastComputeBridgeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyFastComputeBridgeDecomposeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyParamHashBridgeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyParamHashBridgeDecomposeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyParamHashBridgeStateRefineGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyParamHashBridgeStateDecomposeGoalsScript.sml`

Role:
- these are the “compute succeeded ⇒ the abstract fast-domain/ParamHash
  obligations hold” bridges.
- they are the intended attachment point for future CakeML proofs of the
  *algorithmic* core (enumeration, hashing, bucketing) combined with explicit
  Atlas/FFI contracts for the semantic primitives.

Additional split (still `cheat`ed):
- file: `formal/hol4/F4FPPVerifyParamHashBridgeStateRefineGoalsScript.sml`
  - `fast_compute_program_succeeds_imp_paramhash_observation_witness`
  - `fast_compute_program_succeeds_imp_paramhash_invariant_on_observation`

Modulo-`atlas_eq` bridge (still `cheat`ed):
- file: `formal/hol4/F4FPPVerifyParamHashBridgeStateDecomposeGoalsScript.sml`
  - `fast_compute_program_succeeds_imp_paramhash_obligations_state_factored_atlas_eq`

Modulo-`atlas_eq` compute bundle (still `cheat`ed):
- file: `formal/hol4/F4FPPVerifyFastComputeBridgeGoalsScript.sml`
  - `fast_compute_program_succeeds_imp_obligations_atlas_eq`

## F. ParamHash state model

- File: `formal/hol4/F4FPPVerifyParamHashStateGoalsScript.sml`

Status:
- The core representation lemma `ph_contains_state_iff_MEM_elems` is now proved
  (no `cheat`) under the invariant and `atlas_eq_is_hol_eq`.
- Two follow-on generalisations are present but currently placeholders:
  - `find_in_bucket_mem_atlas_eq_imp_SOME` (pure list reasoning about
    `find_in_bucket` when membership is modulo `atlas_eq`).
  - `mem_atlas_eq_elems_imp_ph_contains_state` (the hard direction for the
    modulo-`atlas_eq` representation lemma; uses bucketing + hash-respects-eq).

Progress notes:
- The forward direction `ph_contains_state_imp_mem_atlas_eq_elems` is proved
  (no `cheat`).
- The combined statement `ph_contains_state_iff_mem_atlas_eq_elems` is derived
  from these two directions, but still `cheat`-tainted via the missing reverse
  lemma above.

Decomposition helper:
- `formal/hol4/F4FPPVerifyParamHashStateAtlasEqDecomposeGoalsScript.sml` provides a
  more explicit decomposition of the missing reverse direction, isolating the
  one remaining pure list lemma:
  - `bucket_has_atlas_eq_imp_find_in_bucket_SOME` (currently `cheat`ed).

Rationale:
- this is “pure data-structure reasoning” and should eventually be eliminated
  (either directly in HOL4, or by importing the analogous CakeML proof and
  relating the models).

## G. Bottom-layer check bridges (per-check decomposition)

- Files:
  - `formal/hol4/F4FPPVerifyGlobalDiracBridgeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeGoalsScript.sml`

Role:
- state that successful execution of each bottom-layer check implies the
  corresponding set-level obligation (`standard_final_ok`, hermitian/unitary,
  twist equivalence, dual closure, etc.).

Status:
- per-check success⇒obligation theorems are placeholders (`cheat`) but the
  recombination lemmas are OK.

Planned discharge:
- shallow proofs that the SML code traverses the right set and calls the right
  predicates, plus FFI contracts for those predicates.

Modulo-`atlas_eq` additions:
- `formal/hol4/F4FPPVerifyGlobalDiracBridgeGoalsScript.sml` now also records a
  cheated bridge `bottom_layer_program_succeeds ⇒ bottom_layer_total_ok_param_set_atlas_eq`,
  intended to cover both the compact rho-seeding branch and the non-compact
  check branch.
- `formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeGoalsScript.sml` adds a
  compact-specific cheated predicate `bl_rho_seeded_ok` and uses it to derive
  the modulo-`atlas_eq` total postcondition `bottom_layer_total_ok_atlas_eq`.

## H. Slow checker bridge

- Files:
  - `formal/hol4/F4FPPVerifySlowBridgeDetailedGoalsScript.sml`
  - `formal/hol4/F4FPPVerifySlowProgramDecomposeBridgeGoalsScript.sml`

Role:
- connect `slow_program_succeeds` to the refined slow obligations
  (`slow_refinement_ok` and `slow_ok_components`).

Status:
- currently placeholders (`cheat`).

Modulo-`atlas_eq` detail:
- `formal/hol4/F4FPPVerifySlowProgramDecomposeBridgeGoalsScript.sml` also adds a
  second missing predicate `slow_missing_atlas_eq` and corresponding bridge
  obligations (`slow_missing_ok_atlas_eq`, `slow_ok_sml_atlas_eq`) to derive the
  more realistic “0 misses” goal `slow_ok_components_atlas_eq`.

## I. Composition layers

Most of the “end-to-end” files are **OK composition** and do not introduce new
`cheat`s; they just chain bridge lemmas and the pure logical theorems:

- `formal/hol4/F4FPPVerifyEndToEndObligationStackGoalsScript.sml`
- `formal/hol4/F4FPPVerifyEndToEndProgramSuccessStackGoalsScript.sml`
- `formal/hol4/F4FPPVerifyEndToEndF4sGoalsScript.sml`

These files are valuable even while `cheat`-tainted because they pin down
exactly what must be shown at each interface boundary.
