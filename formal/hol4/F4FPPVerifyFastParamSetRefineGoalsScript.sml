(*
  File: formal/hol4/F4FPPVerifyFastParamSetRefineGoalsScript.sml

  Purpose
  - Refine the single predicate `fast_param_set_ok g` (from
    `F4FPPVerifyFastParamSetGoalsTheory`) into smaller obligations that match
    how the SML code is structured around `ParamHash`:

      (1) the fast program exposes a list of parameters (in insertion order),
          which we model as `fast_list g`;
      (2) the bottom-layer checker uses a `contains` method on the underlying
          hash structure.

  Concretely, we split `fast_param_set_ok g` into:
  - `fast_param_set_list_ok g`:
      the `param_set`’s list is exactly `fast_list g`;
  - `fast_param_set_contains_ok g`:
      the `param_set`’s `contains` agrees with membership in `U_fast g`.

  These are the obligations that should ultimately be discharged by:
  - a correctness theorem about `ParamHash.list` (returns all stored elements),
  - soundness/completeness theorems about `ParamHash.contains` (no false
    positives/negatives with respect to the abstract set view),
  - plus any FFI specs needed for `AtlasParam.eq` and `AtlasParam.hash_mod`.

  Status
  - “OK”: definitional unfolding and set/list reasoning only.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPBottomLayerParamSetGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;

val _ = new_theory "F4FPPVerifyFastParamSetRefineGoals";

Definition fast_param_set_list_ok_def:
  fast_param_set_list_ok g <=>
    ps_list (fast_param_set g) = fast_list g
End

Definition fast_param_set_contains_ok_def:
  fast_param_set_contains_ok g <=>
    !p. ps_contains (fast_param_set g) p <=> p IN U_fast g
End

(* A convenient single bundle matching the SML interface contract. *)
Definition fast_param_set_rep_ok_def:
  fast_param_set_rep_ok g <=>
    fast_param_set_list_ok g /\ fast_param_set_contains_ok g
End

Theorem fast_param_set_rep_ok_imp_fast_param_set_ok:
  !g. fast_param_set_rep_ok g ==> fast_param_set_ok g
Proof
  rw[fast_param_set_rep_ok_def, fast_param_set_ok_def,
     fast_param_set_list_ok_def, fast_param_set_contains_ok_def,
     param_set_rep_ok_def, U_fast_def]
QED

val _ = export_theory ();

