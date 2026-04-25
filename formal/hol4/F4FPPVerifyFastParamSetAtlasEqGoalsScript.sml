(*
  File: formal/hol4/F4FPPVerifyFastParamSetAtlasEqGoalsScript.sml

  Purpose
  - Provide a modulo-`atlas_eq` analogue of `F4FPPVerifyFastParamSetGoalsTheory`
    that matches how the concrete SML `ParamHash.contains` behaves: membership
    is tested via Atlas semantic equality `atlas_eq`, i.e. by
    `mem_set_atlas_eq p U_fast` rather than `p IN U_fast`.

  What this theory adds
  - `fast_param_set_ok_atlas_eq g`: the abstract `fast_param_set g` satisfies
    `param_set_rep_ok_atlas_eq` w.r.t. the abstract output set `U_fast g`.
  - Glue lemmas showing how a successful bottom-layer run over the param_set
    implies the modulo-`atlas_eq` bottom-layer postconditions.

  Status
  - “OK”: definitional unfolding and composition of previously proved lemmas.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPBottomLayerParamSetAtlasEqGoalsTheory;
open F4FPPBottomLayerGoalsAtlasEqTheory;

val _ = new_theory "F4FPPVerifyFastParamSetAtlasEqGoals";

(* Core representation obligation, but with `contains` interpreted modulo
   `atlas_eq` (via `mem_set_atlas_eq`). *)
Definition fast_param_set_ok_atlas_eq_def:
  fast_param_set_ok_atlas_eq g <=>
    param_set_rep_ok_atlas_eq (fast_param_set g) (U_fast g)
End

Theorem fast_param_set_ok_atlas_eq_imp_list_set_eq:
  !g. fast_param_set_ok_atlas_eq g ==> set (ps_list (fast_param_set g)) = U_fast g
Proof
  rw[fast_param_set_ok_atlas_eq_def, param_set_rep_ok_atlas_eq_def]
QED

Theorem fast_param_set_ok_atlas_eq_and_bottom_layer_ok_param_set_imp_bottom_layer_ok_atlas_eq:
  !g dirac.
    atlas_eq_equiv /\ atlas_eq_congruent_bottom_layer /\
    fast_param_set_ok_atlas_eq g /\
    bottom_layer_ok_param_set g dirac (fast_param_set g) ==>
      bottom_layer_ok_atlas_eq g dirac (U_fast g)
Proof
  rw[fast_param_set_ok_atlas_eq_def]
  \\ metis_tac[bottom_layer_ok_param_set_imp_bottom_layer_ok_atlas_eq]
QED

Theorem fast_param_set_ok_atlas_eq_and_bottom_layer_total_ok_param_set_atlas_eq_imp_bottom_layer_total_ok_atlas_eq:
  !g dirac.
    atlas_eq_equiv /\ atlas_eq_congruent_bottom_layer /\
    fast_param_set_ok_atlas_eq g /\
    bottom_layer_total_ok_param_set_atlas_eq g dirac (fast_param_set g) ==>
      bottom_layer_total_ok_atlas_eq g dirac (U_fast g)
Proof
  rw[fast_param_set_ok_atlas_eq_def]
  \\ metis_tac[bottom_layer_total_ok_param_set_atlas_eq_imp_bottom_layer_total_ok_atlas_eq]
QED

val _ = export_theory ();

