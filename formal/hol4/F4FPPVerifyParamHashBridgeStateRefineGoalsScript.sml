(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateRefineGoalsScript.sml

  Purpose
  - Add an explicit intermediate obligation between “the fast compute phase
    ran” and the factored ParamHash correctness predicate `paramhash_rep_ok g`.
  - Concretely, we introduce a predicate `paramhash_state_ok g` asserting that
    the concrete ParamHash object can be explained by an abstract state `s`
    satisfying the state invariant `ph_invariant`, and whose observations are
    exactly `paramhash_list g` and `paramhash_contains g`.

  Why this matters
  - In `F4FPPVerifyParamHashBridgeDecomposeGoalsTheory` we currently record a
    cheated lemma:

      `fast_compute_program_succeeds g ⇒ paramhash_rep_ok g`.

    But the “right” proof is typically two-stage:
      (1) show the imperative hash-table state satisfies an invariant, and
      (2) show that invariant implies `contains` agrees with list membership.

    This file names (1) as its own obligation, so it can later be discharged
    by a CakeML/translator proof.

  Status
  - The refinement lemma `paramhash_state_ok ⇒ paramhash_rep_ok` is OK and now
    depends on a fully proved state lemma `ph_contains_state_iff_MEM_elems`
    from `F4FPPVerifyParamHashStateGoalsTheory` (no `cheat`).
  - The bridge from program success to `paramhash_state_ok` is recorded and
    `cheat`ed.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashStateGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateRefineGoals";

(* Existence of an abstract state explaining the observed list/contains view. *)
Definition paramhash_state_ok_def:
  paramhash_state_ok g <=>
    ?s. ph_invariant s /\ paramhash_observes_state g s
End

(* A finer-grained decomposition of `paramhash_state_ok` that matches how we
   expect the eventual proof to be organised:
   - `paramhash_observation_witness` is “plumbing”: there *exists* some abstract
     state whose observations match the concrete `list/contains` view.
   - `paramhash_invariant_on_observation` is “data-structure reasoning”: any
     such observed state satisfies the invariant. *)
Definition paramhash_observation_witness_def:
  paramhash_observation_witness g <=>
    ?s. paramhash_observes_state g s
End

Definition paramhash_invariant_on_observation_def:
  paramhash_invariant_on_observation g <=>
    !s. paramhash_observes_state g s ==> ph_invariant s
End

Theorem paramhash_observation_witness_and_invariant_imp_state_ok:
  !g.
    paramhash_observation_witness g /\ paramhash_invariant_on_observation g ==>
      paramhash_state_ok g
Proof
  rw[paramhash_state_ok_def, paramhash_observation_witness_def,
     paramhash_invariant_on_observation_def]
  \\ metis_tac[]
QED

Theorem paramhash_state_ok_imp_paramhash_rep_ok:
  !g.
    atlas_eq_is_hol_eq /\ atlas_hash_range /\ paramhash_state_ok g ==> paramhash_rep_ok g
Proof
  rw[paramhash_state_ok_def]
  \\ metis_tac[paramhash_observes_state_and_invariant_imp_paramhash_rep_ok]
QED

(* Bridge obligation: fast compute success implies the existence of such a
   witness state.  This is where a future CakeML/translator proof will attach. *)
Theorem fast_compute_program_succeeds_imp_paramhash_state_ok:
  !g. fast_compute_program_succeeds g ==> paramhash_state_ok g
Proof
  (*
    Intended proof (later, without `cheat`):
    - identify a concrete ParamHash post-state after `computeAllIntoParamHash`,
    - establish `ph_invariant` for that state,
    - show `ParamHash.list` and `ParamHash.contains` correspond to
      `paramhash_list g` and `paramhash_contains g`.
  *)
  cheat
QED

(* Separate bridge obligations corresponding to the decomposed view above. *)
Theorem fast_compute_program_succeeds_imp_paramhash_observation_witness:
  !g. fast_compute_program_succeeds g ==> paramhash_observation_witness g
Proof
  (*
    Intended proof (later, without `cheat`):
    - connect the concrete ParamHash heap object to an abstract state `s`
      satisfying `paramhash_observes_state`.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_invariant_on_observation:
  !g. fast_compute_program_succeeds g ==> paramhash_invariant_on_observation g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show that any abstract state consistent with the observed `list/contains`
      view must satisfy the invariant; this is where CakeML/translator proofs
      about the build process and invariant preservation can attach.
  *)
  cheat
QED

(* Recomposing the split bridge obligations to recover `paramhash_state_ok`. *)
Theorem fast_compute_program_succeeds_imp_paramhash_state_ok_decomposed:
  !g.
    fast_compute_program_succeeds g ==>
      paramhash_state_ok g
Proof
  rpt strip_tac
  \\ match_mp_tac paramhash_observation_witness_and_invariant_imp_state_ok
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_paramhash_observation_witness
      , fast_compute_program_succeeds_imp_paramhash_invariant_on_observation
      ]
QED

(* A refinement-friendly variant of the original bridge lemma in
   `F4FPPVerifyParamHashBridgeDecomposeGoalsTheory`: if the compute phase
   succeeds and we assume the simplifying hash/equality contracts, then
   representation correctness follows once we discharge `paramhash_state_ok`. *)
Theorem fast_compute_program_succeeds_imp_paramhash_rep_ok_via_state:
  !g.
    atlas_eq_is_hol_eq /\ atlas_hash_range /\ fast_compute_program_succeeds g ==>
      paramhash_rep_ok g
Proof
  rpt strip_tac
  \\ match_mp_tac paramhash_state_ok_imp_paramhash_rep_ok
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_state_ok]
QED

val _ = export_theory ();
