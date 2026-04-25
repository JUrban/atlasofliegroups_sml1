(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildGoalsScript.sml

  Purpose
  - Introduce a *more concrete* state-level bridge obligation for ParamHash:
    instead of merely asserting “there exists some abstract state `s` observed by
    `(paramhash_list,paramhash_contains)` and satisfying `ph_invariant`”, we
    record an intended *reference construction* of such an `s`.
  - The reference construction is the pure functional build model from
    `F4FPPVerifyParamHashStateBuildGoalsTheory`:
      `ph_build_from_create_state m ps`.

  Why this matters
  - For a future CakeML/translator proof, it is helpful to have a single,
    explicit “this is the state you get if you start empty and insert the
    elements you saw” witness, rather than quantifying over arbitrary `s`.
  - This file does not prove anything about the *imperative* SML heap state
    produced by the real ParamHash implementation.  Instead, it:
      (1) names the obligation `paramhash_build_witness g`, and
      (2) shows that this obligation is sufficient to discharge the existing
          `paramhash_state_ok g` goal from
          `F4FPPVerifyParamHashBridgeStateRefineGoalsTheory`.

  Intended future use (high level)
  - A CakeML/translator proof should establish:
        `fast_compute_program_succeeds g ==> paramhash_build_witness g`
    by relating the concrete heap state to a pure trace `ps` and bucket count
    `m`, then showing the observations match
    `ph_build_from_create_state m ps`.
  - Once that exists, the remaining HOL4 development can use this file to
    derive `paramhash_state_ok g` and then `paramhash_rep_ok g` (and the
    `paramhash_ok` bundle) through already-existing refinement lemmas.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyParamHashStateGoalsTheory;
open F4FPPVerifyParamHashStateBuildGoalsTheory;
open F4FPPVerifyParamHashBridgeStateRefineGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildGoals";

(* Concrete obligation: the ParamHash observations are explained by a state
   that is *constructed* by the pure build model. *)
Definition paramhash_build_witness_def:
  paramhash_build_witness g <=>
    ?m ps. m <> 0 /\ paramhash_observes_state g (ph_build_from_create_state m ps)
End

Theorem paramhash_build_witness_imp_paramhash_observation_witness:
  !g. paramhash_build_witness g ==> paramhash_observation_witness g
Proof
  rw[paramhash_build_witness_def, paramhash_observation_witness_def]
  \\ metis_tac[]
QED

(* Key payoff: under the (minimal) hash-range contract, a build witness gives an
   invariant state witness, hence `paramhash_state_ok`. *)
Theorem atlas_hash_range_and_paramhash_build_witness_imp_paramhash_state_ok:
  !g. atlas_hash_range /\ paramhash_build_witness g ==> paramhash_state_ok g
Proof
  rpt gen_tac
  \\ strip_tac
  \\ rw[paramhash_state_ok_def]
  \\ qpat_x_assum `paramhash_build_witness g` mp_tac
  \\ simp[paramhash_build_witness_def]
  \\ disch_then (qx_choose_then `m` (qx_choose_then `ps` strip_assume_tac))
  \\ qexists_tac `ph_build_from_create_state m ps`
  \\ conj_tac
  >- (
    match_mp_tac ph_build_from_create_state_invariant
    \\ simp[])
  \\ simp[]
QED

val _ = export_theory ();

