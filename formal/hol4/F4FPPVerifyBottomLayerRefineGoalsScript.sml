(*
  File: formal/hol4/F4FPPVerifyBottomLayerRefineGoalsScript.sml

  Purpose
  - Provide a small refinement step connecting:
      - the *set-level* bottom-layer postcondition
          `bottom_layer_total_ok g dirac (U_fast g)`
        from `F4FPPBottomLayerGoalsTheory`, and
      - the *list-level* “iterate and check” skeleton
          `bottom_layer_ok_list g dirac (fast_list g)`
        from `F4FPPBottomLayerAlgGoalsTheory`.

  Motivation
  - The SML implementation in `FPP_globalDirac.sml` iterates a list of params
    (from `ParamHash.list`/`BigUnitaryHash.list`) and checks each property.
  - Our abstract goal layer (`F4FPPVerifyGoalsTheory`) represents the fast
    computed set as `U_fast g = set (fast_list g)`.
  - This theory proves the simple rewriting lemma that lets future bridge
    proofs target list-based obligations.

  Status
  - “OK”: purely definitional unfolding and previously proved equivalences.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPBottomLayerGoalsTheory;
open F4FPPBottomLayerAlgGoalsTheory;

val _ = new_theory "F4FPPVerifyBottomLayerRefineGoals";

(* List-based bottom-layer obligation for the fast output list. *)
Definition fast_bottom_layer_ok_list_def:
  fast_bottom_layer_ok_list g (dirac:bool) <=>
    bottom_layer_ok_list g dirac (fast_list g)
End

Theorem fast_bottom_layer_ok_list_iff_set_level:
  !g dirac.
    fast_bottom_layer_ok_list g dirac <=>
    bottom_layer_ok g dirac (U_fast g)
Proof
  simp[fast_bottom_layer_ok_list_def, U_fast_def, bottom_layer_ok_list_iff_set]
QED

(* For non-compact groups, `bottom_layer_total_ok` reduces to `bottom_layer_ok`. *)
Theorem fast_bottom_layer_ok_list_iff_total_ok_noncompact:
  !g dirac.
    ~group_is_compact g ==>
      (fast_bottom_layer_ok_list g dirac <=>
       bottom_layer_total_ok g dirac (U_fast g))
Proof
  simp[bottom_layer_total_ok_def, fast_bottom_layer_ok_list_iff_set_level]
QED

val _ = export_theory ();

