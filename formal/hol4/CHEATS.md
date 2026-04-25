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

- Files:
  - `formal/hol4/F4FPPVerifyAtlasFFIContractsGoalsScript.sml` (definitions + internal consequences)
  - `formal/hol4/F4FPPVerifyAtlasFFIContractsCheatsGoalsScript.sml` (the top-level “contracts hold” assumption)
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

- Files:
  - `formal/hol4/F4FPPVerifySMLBridgeGoalsScript.sml` (introduces the abstract success predicates)
  - `formal/hol4/F4FPPVerifySMLBridgeCheatsGoalsScript.sml` (records the `cheat`ed bridge theorems)
- Role: `fast_program_succeeds` / `slow_program_succeeds` are abstract
  predicates; success should imply the abstract obligations.
- Status:
  - the “whole-program success ⇒ obligations” lemmas are now proved by
    composition (no new `cheat` introduced in
    `formal/hol4/F4FPPVerifySMLBridgeCheatsGoalsScript.sml`), but they remain
    CHEAT-tainted because they depend on smaller bridge lemmas that are still
    placeholders.
  - this keeps the remaining trusted surface *below* the top-level glue,
    making it easier to chip away at bridge obligations one-by-one.

Planned discharge:
- refactor SML to be I/O-free and return structured results,
- then prove (in CakeML or a shallow HOL model) that successful evaluation
  implies the stated obligations.

## C. Refined-main route bridges (program success ⇒ refined obligations)

- Files:
  - `formal/hol4/F4FPPVerifyRefinedBridgeGoalsScript.sml` (OK composition)
  - `formal/hol4/F4FPPVerifyRefinedBridgeCheatsGoalsScript.sml` (records the `cheat`ed bridge theorems)
- Placeholders:
  - `fast_program_succeeds_imp_refined_fast_obligations`
  - `fast_program_succeeds_imp_refined_fast_obligations_atlas_eq`
  - `slow_program_succeeds_imp_refined_slow_obligations`
  - `slow_program_succeeds_imp_refined_slow_obligations_atlas_eq`

Role:
- connects concrete success predicates to the obligation bundles used by the
  refined-main theorems, including the modulo-`atlas_eq` variant.

## D. Fast program phase split

- Files:
  - `formal/hol4/F4FPPVerifyFastProgramSplitBridgeGoalsScript.sml` (OK composition)
  - `formal/hol4/F4FPPVerifyFastProgramSplitBridgeCheatsGoalsScript.sml` (split lemma)
- Placeholders:
  - `fast_program_succeeds_imp_fast_compute_program_succeeds`
  - `fast_program_succeeds_imp_bottom_layer_program_succeeds`
  - `fast_program_succeeds_imp_phase_success` (derived, no new `cheat`)

Role:
- makes the fast program’s control-flow (“compute phase; then bottom-layer
  checks”) explicit in the formal decomposition.

## E. Fast compute phase ⇒ obligations

- Files:
  - `formal/hol4/F4FPPVerifyFastComputeBridgeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyFastComputeBridgeCheatsGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyFastComputeBridgeDecomposeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyFastComputeBridgeDecomposeCheatsGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyParamHashBridgeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyParamHashBridgeCheatsGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyParamHashBridgeDecomposeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyParamHashBridgeDecomposeCheatsGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyParamHashBridgeStateRefineGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyParamHashBridgeStateRefineCheatsGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyParamHashBridgeStateDecomposeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoalsScript.sml`

Role:
- these are the “compute succeeded ⇒ the abstract fast-domain/ParamHash
  obligations hold” bridges.
- they are the intended attachment point for future CakeML proofs of the
  *algorithmic* core (enumeration, hashing, bucketing) combined with explicit
  Atlas/FFI contracts for the semantic primitives.

Additional split (still `cheat`ed):
- file: `formal/hol4/F4FPPVerifyParamHashBridgeStateRefineCheatsGoalsScript.sml`
  - `fast_compute_program_succeeds_imp_paramhash_observation_witness`
  - `fast_compute_program_succeeds_imp_paramhash_ok_on_observation`
  - `fast_compute_program_succeeds_imp_paramhash_bucketed_on_observation`
  - `fast_compute_program_succeeds_imp_paramhash_covered_on_observation`
  - `fast_compute_program_succeeds_imp_paramhash_invariant_on_observation` (derived, no new `cheat`)
  - `fast_compute_program_succeeds_imp_paramhash_state_ok` (derived, no new `cheat`)

Fast-domain split (still `cheat`ed):
- file: `formal/hol4/F4FPPVerifyFastComputeBridgeDecomposeCheatsGoalsScript.sml`
  - `fast_compute_program_succeeds_imp_fast_domain_sound`
  - `fast_compute_program_succeeds_imp_fast_domain_complete`
  - `fast_compute_program_succeeds_imp_fast_domain_is_pruned` (derived, no new `cheat`)
  - `fast_compute_program_succeeds_imp_fast_witnessed_pruned_exists`
  - `fast_compute_program_succeeds_imp_fast_unitary_set`
  - `fast_compute_program_succeeds_imp_fast_witnessed_pruned` (derived, no new `cheat`)

Decomposition helper (OK):
- `formal/hol4/F4FPPVerifyFastPruneDecomposeGoalsScript.sml` introduces the
  inclusion-direction obligations (`fast_domain_sound`, `fast_domain_complete`)
  and the split witness obligations (`fast_witnessed_pruned_exists`,
  `fast_unitary_set`) and provides the recombination lemmas.

Note:
- `formal/hol4/F4FPPVerifyFastComputeBridgeCheatsGoalsScript.sml` no longer
  introduces an additional top-level `cheat` for
  `fast_compute_program_succeeds_imp_obligations`; it is now derived from the
  factored bridge lemma `fast_compute_program_succeeds_imp_fast_compute_obligations_factored`.

Modulo-`atlas_eq` bridge (still `cheat`ed):
- file: `formal/hol4/F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoalsScript.sml`
  - `fast_compute_program_succeeds_imp_paramhash_obligations_state_factored_atlas_eq`
  - (now derived from smaller cheated obligations for wiring/state/stores)

Canonical build-state + state-based stores bundle:
- file: `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresDecomposeCheatsGoalsScript.sml`
  - `fast_compute_program_succeeds_imp_paramhash_build_stores_U_fast_atlas_eq`
    (intended translator target for “stores-U-fast” stated against `ph_set`)
  - `fast_compute_program_succeeds_imp_paramhash_obligations_build_state_stores_factored_atlas_eq`
    (packages wiring + `paramhash_build_state_ok` + state-based stores)

Trace-level stores upgrade (optional target):
- file: `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceCheatsGoalsScript.sml`
  - `atlas_hash_eq_ok_and_paramhash_build_m_ok_and_build_ps_stores_imp_build_stores`
    (if `U_fast` matches the build trace list, then it matches `ph_set` of the build state)
  - `atlas_hash_eq_ok_and_paramhash_build_m_ok_and_build_stores_imp_build_ps_stores`
    (converse direction; useful if one first proves the state-based stores predicate)
- file: `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceDecomposeCheatsGoalsScript.sml`
  - `atlas_hash_eq_ok_and_build_state_trace_stores_factored_atlas_eq_imp_paramhash_obligations_factored_atlas_eq`
    (packages wiring + build_state_ok + trace-level stores into extensional obligations)
- file: `formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceBridgeCheatsGoalsScript.sml`
  - `fast_compute_program_succeeds_imp_paramhash_build_ps_stores_U_fast_atlas_eq`
    (intended translator target: relate compute success to `U_fast ~ set(trace)` directly)
- file: `formal/hol4/F4FPPVerifyParamHashStateBuildSetGoalsScript.sml`
  - provides the pure build-set lemma `ph_build_from_create_state_set_atlas_eq_set_ps` (currently CHEAT)

Modulo-`atlas_eq` compute bundle (still `cheat`ed):
- file: `formal/hol4/F4FPPVerifyFastComputeBridgeCheatsGoalsScript.sml`
  - `fast_compute_program_succeeds_imp_obligations_atlas_eq`

## F. ParamHash state model

- File: `formal/hol4/F4FPPVerifyParamHashStateGoalsScript.sml`

Status:
- The core representation lemma `ph_contains_state_iff_MEM_elems` is now proved
  (no `cheat`) under the invariant and `atlas_eq_is_hol_eq`.
- The more realistic modulo-`atlas_eq` representation lemma is now also proved
  (no `cheat`) under `atlas_hash_eq_ok` + `ph_invariant`:
  - `ph_contains_state_iff_mem_atlas_eq_elems`.

Progress notes:
- The forward direction `ph_contains_state_imp_mem_atlas_eq_elems` is proved
  (no `cheat`).
- The reverse direction `mem_atlas_eq_elems_imp_ph_contains_state` is now proved
  (no `cheat`), using:
  - a pure completeness lemma for `find_in_bucket` modulo `atlas_eq`
    (`find_in_bucket_mem_atlas_eq_imp_SOME`), and
  - the `ph_covered`/`ph_bucketed` plumbing plus `atlas_hash_respects_eq`.

Decomposition helper:
- `formal/hol4/F4FPPVerifyParamHashStateAtlasEqDecomposeGoalsScript.sml` provides a
  more explicit decomposition of the reverse direction; it is now entirely OK
  (no `cheat`) and can be used as a readable guide for the proof structure.

Rationale:
- this is “pure data-structure reasoning” and should eventually be eliminated
  (either directly in HOL4, or by importing the analogous CakeML proof and
  relating the models).

## G. Bottom-layer check bridges (per-check decomposition)

- Files:
  - `formal/hol4/F4FPPVerifyGlobalDiracBridgeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeCheatsGoalsScript.sml`

Role:
- state that successful execution of each bottom-layer check implies the
  corresponding set-level obligation (`standard_final_ok`, hermitian/unitary,
  twist equivalence, dual closure, etc.).

Status:
- per-check success⇒obligation theorems live in the isolated `*Cheats*` theory
  and are placeholders (`cheat`), but the decomposition/recombination lemmas
  are OK.

Planned discharge:
- shallow proofs that the SML code traverses the right set and calls the right
  predicates, plus FFI contracts for those predicates.

Modulo-`atlas_eq` additions:
- `formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeCheatsGoalsScript.sml` records a
  cheated bridge `bottom_layer_program_succeeds ⇒ bottom_layer_total_ok_param_set_atlas_eq`,
  intended to cover both the compact rho-seeding branch and the non-compact
  check branch.
- For HOL equality (non-modulo), `formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeCheatsGoalsScript.sml`
  now also provides the convenience composition lemma
  `bottom_layer_program_succeeds_and_fast_param_set_ok_imp_total_ok_decomposed`,
  which covers both branches by cases on `group_is_compact g`.
- `formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeGoalsScript.sml` adds a
  compact-specific predicate `bl_rho_seeded_ok` and uses it to derive
  the modulo-`atlas_eq` total postcondition `bottom_layer_total_ok_atlas_eq`.

## H. Slow checker bridge

- Files:
  - `formal/hol4/F4FPPVerifySlowBridgeDetailedGoalsScript.sml`
  - `formal/hol4/F4FPPVerifySlowBridgeDetailedCheatsGoalsScript.sml`
  - `formal/hol4/F4FPPVerifySlowProgramDecomposeBridgeGoalsScript.sml`
  - `formal/hol4/F4FPPVerifySlowProgramDecomposeBridgeCheatsGoalsScript.sml`
  - `formal/hol4/F4FPPVerifySlowRefinementBridgeDecomposeCheatsGoalsScript.sml`

Role:
- connect `slow_program_succeeds` to the refined slow obligations
  (`slow_refinement_ok` and `slow_ok_components`).

Status:
- bridge implications from `slow_program_succeeds` live in the isolated
  `*Cheats*` theories and are placeholders (`cheat`); the decomposition lemmas
  are OK.

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
