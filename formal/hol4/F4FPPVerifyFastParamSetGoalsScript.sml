(*
  File: formal/hol4/F4FPPVerifyFastParamSetGoalsScript.sml

  Purpose
  - Introduce an explicit abstract “fast program param_set” object and connect
    it to the existing fast-set model `U_fast g`.

  Motivation
  - The fast verifier (`VerifyF4FPP.sml`) computes its output set inside a
    mutable hash structure (`ParamHash.t`), and the bottom-layer checker
    (`FPP_globalDirac.sml`) consumes that structure through the abstract
    interface:

      { list : unit -> param list
      , contains : param -> bool }

  - In HOL4, the main goal stack uses:
      `U_fast g = set (fast_list g)`
    as the abstract output set.
  - To bridge these worlds cleanly, we name the `param_set` view explicitly and
    isolate the single key obligation:
      the `contains` method agrees with membership in the set enumerated by
      `list`.

  Status
  - “OK”: all content here is definitional and composes earlier lemmas.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPBottomLayerParamSetGoalsTheory;
open F4FPPBottomLayerGoalsTheory;

val _ = new_theory "F4FPPVerifyFastParamSetGoals";

(* Abstract param_set view of the fast program’s hash structure. *)
val _ = new_constant ("fast_param_set", ``:group -> param_set``);

(* Core representation obligation: the param_set represents exactly `U_fast g`. *)
Definition fast_param_set_ok_def:
  fast_param_set_ok g <=>
    param_set_rep_ok (fast_param_set g) (U_fast g)
End

Theorem fast_param_set_ok_imp_list_set_eq:
  !g. fast_param_set_ok g ==> set (ps_list (fast_param_set g)) = U_fast g
Proof
  rw[fast_param_set_ok_def, param_set_rep_ok_def]
QED

Theorem fast_param_set_ok_and_bottom_layer_ok_param_set_imp_bottom_layer_ok:
  !g dirac.
    fast_param_set_ok g /\ bottom_layer_ok_param_set g dirac (fast_param_set g) ==>
      bottom_layer_ok g dirac (U_fast g)
Proof
  rw[fast_param_set_ok_def]
  \\ metis_tac[bottom_layer_ok_param_set_iff_set]
QED

val _ = export_theory ();

