(*
  File: formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeGoalsScript.sml

  Purpose
  - Decompose the (currently cheated) GlobalDirac bridge
      `bottom_layer_program_succeeds g dirac ⇒ bottom_layer_ok_param_set ...`
    into smaller per-check bridge obligations that mirror the structure of
    `atlas-scripts-sml/FPP_globalDirac.sml`:

      standard/final
      lambda-table consistency (F4 only)
      twist-equivalence
      hermitian
      (optional) unitary, guarded by `dirac`
      contragredient closure (unitary dual symmetry)

  Why this matters
  - It lets us refine the proof story check-by-check (and even discharge some
    checks independently), rather than replacing one giant `cheat` later.

  Status
  - The per-check bridge lemmas are recorded and `cheat`ed.
  - The final “recombination” lemma is OK composition.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPBottomLayerParamSetGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;
open F4FPPVerifyGlobalDiracBridgeGoalsTheory;
open F4FPPBottomLayerGoalsTheory;

val _ = new_theory "F4FPPVerifyGlobalDiracBridgeDecomposeGoals";

(* --- Per-check obligations, phrased directly as `bottom_layer_ok_param_set` conjuncts. --- *)

Definition bl_standard_final_ok_def:
  bl_standard_final_ok g <=>
    standard_final_list_ok (ps_list (fast_param_set g))
End

Definition bl_lambda_table_ok_def:
  bl_lambda_table_ok g <=>
    lambda_table_list_ok g (ps_list (fast_param_set g))
End

Definition bl_twist_equiv_ok_def:
  bl_twist_equiv_ok g <=>
    twist_equiv_list_ok (ps_list (fast_param_set g))
End

Definition bl_hermitian_ok_def:
  bl_hermitian_ok g <=>
    hermitian_list_ok (ps_list (fast_param_set g))
End

Definition bl_unitary_if_ok_def:
  bl_unitary_if_ok g dirac <=>
    unitary_if_list dirac (ps_list (fast_param_set g))
End

Definition bl_dual_closed_ok_def:
  bl_dual_closed_ok g <=>
    dual_closed_contains_ok (fast_param_set g)
End

(* --- Bridge lemmas: success implies each check obligation (CHEATED). --- *)

Theorem bottom_layer_program_succeeds_imp_bl_standard_final_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_standard_final_ok g
Proof
  cheat
QED

Theorem bottom_layer_program_succeeds_imp_bl_lambda_table_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_lambda_table_ok g
Proof
  cheat
QED

Theorem bottom_layer_program_succeeds_imp_bl_twist_equiv_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_twist_equiv_ok g
Proof
  cheat
QED

Theorem bottom_layer_program_succeeds_imp_bl_hermitian_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_hermitian_ok g
Proof
  cheat
QED

Theorem bottom_layer_program_succeeds_imp_bl_unitary_if_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_unitary_if_ok g dirac
Proof
  cheat
QED

Theorem bottom_layer_program_succeeds_imp_bl_dual_closed_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_dual_closed_ok g
Proof
  cheat
QED

(* --- Recombination: per-check obligations imply the full `bottom_layer_ok_param_set`. --- *)

Theorem bl_checks_imp_bottom_layer_ok_param_set:
  !g dirac.
    bl_standard_final_ok g /\
    bl_lambda_table_ok g /\
    bl_twist_equiv_ok g /\
    bl_hermitian_ok g /\
    bl_unitary_if_ok g dirac /\
    bl_dual_closed_ok g ==>
      bottom_layer_ok_param_set g dirac (fast_param_set g)
Proof
  rw
    [ bottom_layer_ok_param_set_def
    , bl_standard_final_ok_def
    , bl_lambda_table_ok_def
    , bl_twist_equiv_ok_def
    , bl_hermitian_ok_def
    , bl_unitary_if_ok_def
    , bl_dual_closed_ok_def
    ]
QED

(* And therefore, success implies the full conjunction (OK composition). *)
Theorem bottom_layer_program_succeeds_imp_bottom_layer_ok_param_set_decomposed:
  !g dirac.
    bottom_layer_program_succeeds g dirac ==>
      bottom_layer_ok_param_set g dirac (fast_param_set g)
Proof
  rpt strip_tac
  \\ match_mp_tac bl_checks_imp_bottom_layer_ok_param_set
  \\ metis_tac
       [ bottom_layer_program_succeeds_imp_bl_standard_final_ok
       , bottom_layer_program_succeeds_imp_bl_lambda_table_ok
       , bottom_layer_program_succeeds_imp_bl_twist_equiv_ok
       , bottom_layer_program_succeeds_imp_bl_hermitian_ok
       , bottom_layer_program_succeeds_imp_bl_unitary_if_ok
       , bottom_layer_program_succeeds_imp_bl_dual_closed_ok
       ]
QED

(* The same downstream consequences as in `F4FPPVerifyGlobalDiracBridgeGoalsTheory`,
   but routed through the per-check decomposition above. *)
Theorem bottom_layer_program_succeeds_and_fast_param_set_ok_imp_bottom_layer_ok_decomposed:
  !g dirac.
    bottom_layer_program_succeeds g dirac /\ fast_param_set_ok g ==>
      bottom_layer_ok g dirac (U_fast g)
Proof
  rpt strip_tac
  \\ match_mp_tac fast_param_set_ok_and_bottom_layer_ok_param_set_imp_bottom_layer_ok
  \\ conj_tac
  >- simp[]
  \\ metis_tac[bottom_layer_program_succeeds_imp_bottom_layer_ok_param_set_decomposed]
QED

Theorem bottom_layer_program_succeeds_and_fast_param_set_ok_imp_total_ok_noncompact_decomposed:
  !g dirac.
    bottom_layer_program_succeeds g dirac /\ fast_param_set_ok g /\ ~group_is_compact g ==>
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  rw[bottom_layer_total_ok_def]
  \\ metis_tac[bottom_layer_program_succeeds_and_fast_param_set_ok_imp_bottom_layer_ok_decomposed]
QED

val _ = export_theory ();
