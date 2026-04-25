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
  - The per-check obligations and recombination lemma are “OK”.
  - The per-check “execution success ⇒ obligation” bridge lemmas (and their
    downstream consequences) are isolated in
    `F4FPPVerifyGlobalDiracBridgeDecomposeCheatsGoalsTheory`.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPBottomLayerParamSetGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;
open F4FPPVerifyGlobalDiracBridgeGoalsTheory;
open F4FPPBottomLayerGoalsTheory;
open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPBottomLayerGoalsAtlasEqTheory;
open F4FPPBottomLayerParamSetAtlasEqGoalsTheory;
open F4FPPVerifyFastParamSetAtlasEqGoalsTheory;

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

(* For compact groups, the pipeline seeds the structure with `rho_set g`. *)
Definition bl_rho_seeded_ok_def:
  bl_rho_seeded_ok g <=>
    !p. p IN rho_set g ==> ps_contains (fast_param_set g) p
End

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

(* Bridge lemmas from `bottom_layer_program_succeeds` to these per-check
   obligations (and the derived downstream consequences) are recorded in:
     `F4FPPVerifyGlobalDiracBridgeDecomposeCheatsGoalsTheory`. *)

val _ = export_theory ();
