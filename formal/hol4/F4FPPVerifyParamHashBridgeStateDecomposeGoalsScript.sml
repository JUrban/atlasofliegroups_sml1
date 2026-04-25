(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateDecomposeGoalsScript.sml

  Purpose
  - Provide an even more detailed decomposition of the ParamHash side of the
    fast-compute bridge, interposing an explicit *state witness* obligation
    (`paramhash_state_ok`) between program success and the extensional
    representation property `paramhash_rep_ok`.

  Big picture
  - In `F4FPPVerifyParamHashBridgeDecomposeGoalsTheory` we factor ParamHash
    obligations into:
      - `paramhash_rep_ok g`            (contains ↔ MEM list)
      - `paramhash_stores_U_fast g`     (list represents `U_fast g`)
      - plus the wiring predicate `fast_param_set_is_paramhash g`.
  - In `F4FPPVerifyParamHashBridgeStateRefineGoalsTheory` we introduce:
      - `paramhash_state_ok g`          (∃ state witness satisfying `ph_invariant`)
      - and a cheated bridge from compute success to `paramhash_state_ok`.

  This theory packages the *state-level* bundle we expect to get from execution:

      wiring + state_ok + stores_U_fast

  and shows that, under explicit Atlas/FFI contracts, it implies the earlier
  extensional bundle.

  Status
  - The logical implication lemmas are OK.
  - The bridge from execution to the state-level bundle is recorded and
    `cheat`ed in `F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoalsTheory`.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateRefineGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateDecomposeGoals";

(* The “state-level” factored bundle: what we expect to prove from concrete
   imperative execution (CakeML/translator-friendly). *)
Definition paramhash_obligations_state_factored_def:
  paramhash_obligations_state_factored g <=>
    fast_param_set_is_paramhash g /\
    paramhash_state_ok g /\
    paramhash_stores_U_fast g
End

(* A state-level bundle phrased modulo Atlas equality: this removes the need to
   assume `atlas_eq_is_hol_eq` when stating the “stores-U-fast” part. *)
Definition paramhash_obligations_state_factored_atlas_eq_def:
  paramhash_obligations_state_factored_atlas_eq g <=>
    fast_param_set_is_paramhash g /\
    paramhash_state_ok g /\
    paramhash_stores_U_fast_atlas_eq g
End

(* Under explicit contracts, the state-level bundle implies the extensional
   representation property `paramhash_rep_ok`. *)
Theorem paramhash_state_factored_imp_paramhash_rep_ok:
  !g.
    atlas_eq_is_hol_eq /\ atlas_hash_range /\ paramhash_obligations_state_factored g ==>
      paramhash_rep_ok g
Proof
  rw[paramhash_obligations_state_factored_def]
  \\ match_mp_tac paramhash_state_ok_imp_paramhash_rep_ok
  \\ simp[]
QED

(* Therefore, under contracts, the state-level bundle implies the extensional
   factored bundle from `F4FPPVerifyParamHashBridgeDecomposeGoalsTheory`. *)
Theorem paramhash_state_factored_imp_paramhash_obligations_factored:
  !g.
    atlas_eq_is_hol_eq /\ atlas_hash_range /\ paramhash_obligations_state_factored g ==>
      paramhash_obligations_factored g
Proof
  rpt strip_tac
  \\ fs[paramhash_obligations_factored_def, paramhash_obligations_state_factored_def]
  \\ metis_tac[paramhash_state_ok_imp_paramhash_rep_ok]
QED

Theorem paramhash_state_factored_atlas_eq_imp_paramhash_obligations_factored_atlas_eq:
  !g.
    atlas_hash_eq_ok /\ paramhash_obligations_state_factored_atlas_eq g ==>
      paramhash_obligations_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ fs[paramhash_obligations_factored_atlas_eq_def, paramhash_obligations_state_factored_atlas_eq_def]
  \\ metis_tac[paramhash_state_ok_imp_paramhash_rep_ok_atlas_eq]
QED

(* Bridge lemmas from `fast_compute_program_succeeds` to these state-level
   bundles are isolated in:
     `F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoalsTheory`. *)

val _ = export_theory ();
