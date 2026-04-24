# HOL4 goals checklist (top-down refinement view)

This document is a “what remains to be proved (eventually)” companion to
`formal/STRATEGY.md`.  It is intentionally **goal-oriented**: it lists the
top-level theorems we ultimately want, the decomposition layers that connect
them, and the remaining “bridge” lemmas that are currently `cheat`ed.

The current development is deliberately structured so that:

- most files are *pure logic / list / set reasoning* (already proved, “OK”),
- the “hard” bits are isolated in a small number of **bridge** theories that
  connect concrete Poly/ML + Atlas execution to abstract predicates.

## 1. Top-level end-to-end statement

The primary end-to-end theorem is phrased in terms of abstract predicates
meaning “the SML programs succeeded”:

- `fast_program_succeeds g dirac`
- `slow_program_succeeds g`

From these, the end goal is:

- `U_slow g (D_slow g) = U_fast g`, and
- `bottom_layer_total_ok g dirac (U_fast g)`.

More realistic variant (modulo Atlas equality):

- `set_atlas_eq (U_slow g (D_slow g)) (U_fast g)`
  - spec-level shape + supporting lemmas:
    - file: `formal/hol4/F4FPPVerifySpecAtlasEqGoalsScript.sml`
      (`complete_rel_atlas_eq`, `sound_wrt_domain_atlas_eq`,
      `sound_and_complete_atlas_eq_gives_set_atlas_eq`)
  - refined-main composition (shows how refined obligations imply the modulo goal):
    - file: `formal/hol4/F4FPPVerifyRefinedMainAtlasEqGoalsScript.sml`
      (`refined_obligations_imply_set_atlas_eq`)
  - refined bridge from program-success predicates (cheat-tainted, but clean structure):
    - file: `formal/hol4/F4FPPVerifyRefinedBridgeGoalsScript.sml`
      (`fast_and_slow_programs_succeed_gives_refined_equivalence_atlas_eq`)
  - concrete specialization for the target group:
    - file: `formal/hol4/F4FPPVerifyEndToEndF4sGoalsScript.sml`
      (`fast_and_slow_programs_succeed_gives_equivalence_atlas_eq_F4s`)
      (also `fast_and_slow_programs_succeed_gives_equivalence_atlas_eq_F4s_fast_atlas_eq`)
  - compatibility lemma (shows modulo statement follows from HOL equality under
    `atlas_eq_is_hol_eq`):
    - file: `formal/hol4/F4FPPVerifyGoalsAtlasEqScript.sml`

There are two main “end-to-end” routes in HOL4:

1. **Refined-main route** (domain + list-model + bottom-layer predicates)
   - `formal/hol4/F4FPPVerifyEndToEndF4sGoalsScript.sml`
   - best when focusing on the slow-domain/list model and the structure of
     the bottom-layer checks.

2. **Obligation-stack route** (more explicit about ParamHash + phase splits)
   - `formal/hol4/F4FPPVerifyEndToEndObligationStackGoalsScript.sml`
   - `formal/hol4/F4FPPVerifyEndToEndProgramSuccessStackGoalsScript.sml`
   - best when focusing on the fast compute pipeline, ParamHash state, and the
     Atlas hash/equality contracts.

Both routes are consistent: they express the same end consequence but expose
different proof boundaries.

## 2. Decomposition tree (what implies what)

At a high level:

1. `fast_program_succeeds`
   - splits into a compute-phase success and a bottom-layer-phase success:
     `formal/hol4/F4FPPVerifyFastProgramSplitBridgeGoalsScript.sml`
2. Compute-phase success implies **compute obligations**
   - `formal/hol4/F4FPPVerifyFastComputeBridgeDecomposeGoalsScript.sml`
   - gives fast-side semantic obligations (`fast_semantic_ok`) and a
     ParamHash-related bundle (currently via a state-factored predicate).
3. Bottom-layer success implies **bottom-layer obligations**
   - decomposed per check in:
     `formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeGoalsScript.sml`
4. Slow success implies **slow obligations**
   - `formal/hol4/F4FPPVerifySlowBridgeDetailedGoalsScript.sml`
5. The obligations imply set equality and bottom-layer postconditions
   - `formal/hol4/F4FPPVerifyEndToEndObligationStackGoalsScript.sml`

The point of the decomposition is that each arrow can be proved/refined
independently (or replaced by a stronger lemma) without rewriting the whole
development.

## 3. Key remaining proof obligations (currently `cheat`ed)

The development is intentionally “CHEAT-tainted at the edges”.  The items
below are the ones we want to eventually replace by real proofs and/or
explicitly justified axioms.

### A. Program-structure (Poly/ML) bridge

These state that “the program ran successfully” implies success of each phase
and the abstract obligations of each phase.

- `fast_program_succeeds_imp_phase_success`
  - file: `formal/hol4/F4FPPVerifyFastProgramSplitBridgeGoalsScript.sml`
  - ideal proof: relate `fast_program_succeeds` to the control-flow of
    `atlas-scripts-sml/VerifyF4FPP.sml` (exceptions, return values).

- `slow_program_succeeds_imp_refined_slow_obligations_detailed`
  - file: `formal/hol4/F4FPPVerifySlowBridgeDetailedGoalsScript.sml`
  - ideal proof: relate `slow_program_succeeds` to the control-flow of
    `atlas-scripts-sml/SimplerVerifyF4FPP.sml`.

### B. Fast compute bridge (ParamHash + enumeration)

These connect the compute phase to the “semantic” fast obligations and the
ParamHash obligations.

- `fast_compute_program_succeeds_imp_fast_compute_domain_ok`
- `fast_compute_program_succeeds_imp_paramhash_obligations_state_factored`
  - file: `formal/hol4/F4FPPVerifyParamHashBridgeStateDecomposeGoalsScript.sml`
  - this is the ideal attachment point for a CakeML proof of the ParamHash
    algorithmic core, plus explicit Atlas hash/equality contracts.

Modulo-`atlas_eq` variant (preferred long-term statement):

- `fast_compute_program_succeeds_imp_paramhash_obligations_state_factored_atlas_eq`
  - file: `formal/hol4/F4FPPVerifyParamHashBridgeStateDecomposeGoalsScript.sml`
  - same bridge shape, but the “stores-U-fast” clause is stated as
    `set_atlas_eq (U_fast g) (set (paramhash_list g))`.

Useful refinement split (for proof engineering):

- `paramhash_observation_witness g`: there exists an abstract state matching the
  observed `list/contains` view (“plumbing”).
- `paramhash_invariant_on_observation g`: any such observed state satisfies the
  invariant (“data-structure reasoning”).
  - file: `formal/hol4/F4FPPVerifyParamHashBridgeStateRefineGoalsScript.sml`

### C. ParamHash state representation lemma (data-structure core)

To reduce the “cheat surface” we want the state-level lemma:

- under invariant + hash/equality contracts, `contains` agrees with
  list-membership:
  - `ph_contains_state p s <=> MEM p s.elems`.

Progress note:

- The full representation lemma `ph_contains_state_iff_MEM_elems` is now proved
  in `formal/hol4/F4FPPVerifyParamHashStateGoalsScript.sml` (no `cheat`).
- The more realistic strengthening (membership modulo Atlas semantic equality
  `atlas_eq`) is developed using:
  - `formal/hol4/F4FPPVerifyAtlasEqListGoalsScript.sml` (`mem_atlas_eq`), and
  - `formal/hol4/F4FPPVerifyAtlasEqSetGoalsScript.sml` (`set_atlas_eq`,
    `atlas_eq_closure`).
  The key state lemma `ph_contains_state_iff_mem_atlas_eq_elems` is stated in
  `formal/hol4/F4FPPVerifyParamHashStateGoalsScript.sml` and is still
  `cheat`-tainted via the missing reverse direction (the forward direction is
  proved).

On the CakeML side, the analogous lemma is *proved* for the pure-state model:

- `formal/cakeml/ParamHashSetGoalsScript.sml` (`ph_rep_ok_iff_MEM_elems`).

The eventual “bridge plan” is:

1. prove the state-level lemma for the CakeML ParamHash model, then
2. relate the concrete SML `ParamHash` (Poly/ML + FFI) to that model, under
   explicit Atlas hash/equality assumptions.

### D. Bottom-layer per-check bridges

Each bottom-layer check has a separate “success ⇒ obligation” lemma in:

- `formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeGoalsScript.sml`

These are currently `cheat`ed and are intended to be discharged by:

- a shallow proof that the SML check traverses exactly the required set and
  calls the corresponding semantic predicate (`is_standard`, `is_unitary`,
  `contragredient`, etc.), plus
- a set of FFI contracts for those primitives.

### E. Atlas/FFI contract layer

The “FFI contract inventory” is:

- `formal/hol4/F4FPPVerifyAtlasFFIContractsGoalsScript.sml`

It introduces:

- `atlas_eq`, `atlas_hash_mod`, `atlas_clone`,
- and bundles (`atlas_hash_eq_ok`, `atlas_ffi_contracts`).

Current approach:

- treat `atlas_ffi_contracts` as an axiom (`atlas_ffi_contracts_hold`) until we
  decide whether to:
  - keep it axiomatic,
  - validate it empirically, or
  - model/verify enough of the C++/FFI to justify it.

## 4. Recommended proof sequence (incremental strengthening)

1. Make the bridge statements more precise (still allowed to `cheat`)
   - refine “program succeeds” into return values + I/O-free functions.
2. Discharge the **pure** obligations in CakeML
   - ParamHash invariants, list/set interfaces, bucketing logic.
3. Replace the HOL4 ParamHash `cheat` with a theorem imported from the CakeML
   development via an explicit abstraction/encoding relation.
4. Gradually tighten the FFI contracts
   - from “all Atlas primitives satisfy these laws” to smaller, more local
     assumptions per check/phase.
