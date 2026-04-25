(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildCheatsGoalsScript.sml

  Purpose
  - Record (currently as `cheat`) the *intended* theorem that connects the
    concrete fast-compute phase execution to the new, more concrete ParamHash
    state obligation `paramhash_build_witness`.

  Background
  - `F4FPPVerifyParamHashBridgeStateRefineCheatsGoalsTheory` already contains a
    family of cheated lemmas of the form:
        `fast_compute_program_succeeds g ==> paramhash_state_ok g`
    decomposed into multiple sub-obligations.
  - The new theory `F4FPPVerifyParamHashBridgeStateBuildGoalsTheory` introduces
    a more *specified* intermediate goal:
        `paramhash_build_witness g`
    which asserts that the ParamHash observations correspond to the *pure*
    build-state model (`ph_build_from_create_state`).

  Intended future proof outline (CakeML/translator)
  - Prove that the imperative ParamHash operations refine the pure model:
      - identify a trace `ps` of attempted insertions (or successful insertions),
      - show the concrete observation `paramhash_list g` equals the resulting
        pure state's `elems`,
      - show `paramhash_contains g` agrees with `ph_contains_state` for that
        same pure state.
  - The details will likely be done in CakeML’s monadic translator style,
    where each imperative step is related to a functional state transformer.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceCheatsGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildCheatsGoals";

Theorem fast_compute_program_succeeds_imp_paramhash_build_witness:
  !g. fast_compute_program_succeeds g ==> paramhash_build_witness g
Proof
  (* Prefer routing via the trace-based canonical model:
       `fast_compute_program_succeeds ==> paramhash_build_state_ok`
     (CHEATED, translator target) and then discharge the existential witness
     with the OK lemma `paramhash_build_state_ok_imp_paramhash_build_witness`. *)
  metis_tac
    [ fast_compute_program_succeeds_imp_paramhash_build_state_ok
    , paramhash_build_state_ok_imp_paramhash_build_witness
    ]
QED

(* A convenient derived bridge, using only the single concrete obligation. *)
Theorem atlas_hash_range_and_fast_compute_program_succeeds_imp_paramhash_state_ok_via_build_witness:
  !g. atlas_hash_range /\ fast_compute_program_succeeds g ==> paramhash_state_ok g
Proof
  rpt strip_tac
  \\ match_mp_tac atlas_hash_range_and_paramhash_build_witness_imp_paramhash_state_ok
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_build_witness]
QED

val _ = export_theory ();
