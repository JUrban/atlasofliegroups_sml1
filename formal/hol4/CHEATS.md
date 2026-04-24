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

## C. Fast program phase split

- File: `formal/hol4/F4FPPVerifyFastProgramSplitBridgeGoalsScript.sml`
- Placeholder:
  - `fast_program_succeeds_imp_phase_success`

Role:
- makes the fast program’s control-flow (“compute phase; then bottom-layer
  checks”) explicit in the formal decomposition.

## D. Fast compute phase ⇒ obligations

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

## E. ParamHash state model

- File: `formal/hol4/F4FPPVerifyParamHashStateGoalsScript.sml`

What is proved:
- `ph_lookup_state_SOME_imp_MEM_elems` (lookup success implies membership)

What is still `cheat`ed:
- `MEM_elems_imp_ph_contains_state` (membership implies lookup success)
- therefore, `ph_contains_state_iff_MEM_elems` is still `CHEAT`-tainted.

Rationale:
- this is “pure data-structure reasoning” and should eventually be eliminated
  (either directly in HOL4, or by importing the analogous CakeML proof and
  relating the models).

## F. Bottom-layer check bridges (per-check decomposition)

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

## G. Slow checker bridge

- Files:
  - `formal/hol4/F4FPPVerifySlowBridgeDetailedGoalsScript.sml`
  - `formal/hol4/F4FPPVerifySlowProgramDecomposeBridgeGoalsScript.sml`

Role:
- connect `slow_program_succeeds` to the refined slow obligations
  (`slow_refinement_ok` and `slow_ok_components`).

Status:
- currently placeholders (`cheat`).

## H. Composition layers

Most of the “end-to-end” files are **OK composition** and do not introduce new
`cheat`s; they just chain bridge lemmas and the pure logical theorems:

- `formal/hol4/F4FPPVerifyEndToEndObligationStackGoalsScript.sml`
- `formal/hol4/F4FPPVerifyEndToEndProgramSuccessStackGoalsScript.sml`
- `formal/hol4/F4FPPVerifyEndToEndF4sGoalsScript.sml`

These files are valuable even while `cheat`-tainted because they pin down
exactly what must be shown at each interface boundary.

