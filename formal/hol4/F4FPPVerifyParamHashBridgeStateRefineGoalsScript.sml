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
    `cheat`ed in `F4FPPVerifyParamHashBridgeStateRefineCheatsGoalsTheory`.
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

(* More realistic equality: extensional correctness modulo `atlas_eq`. *)
Theorem paramhash_state_ok_imp_paramhash_rep_ok_atlas_eq:
  !g.
    atlas_hash_eq_ok /\ paramhash_state_ok g ==> paramhash_rep_ok_atlas_eq g
Proof
  rpt gen_tac
  \\ strip_tac
  \\ qpat_x_assum `paramhash_state_ok g` mp_tac
  \\ simp[paramhash_state_ok_def]
  \\ strip_tac
  \\ rw[paramhash_rep_ok_atlas_eq_def]
  \\ fs[paramhash_observes_state_def]
  \\ simp[ph_contains_state_iff_mem_atlas_eq_elems]
QED

(* Bridge lemmas connecting `fast_compute_program_succeeds` to these
   state-level obligations are isolated in:
     `F4FPPVerifyParamHashBridgeStateRefineCheatsGoalsTheory`. *)

val _ = export_theory ();
