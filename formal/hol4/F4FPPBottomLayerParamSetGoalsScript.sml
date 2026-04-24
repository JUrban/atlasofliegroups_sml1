(*
  File: formal/hol4/F4FPPBottomLayerParamSetGoalsScript.sml

  Purpose
  - Model the `param_set` interface used by `atlas-scripts-sml/FPP_globalDirac.sml`:

      type param_set = {list: unit -> param list, contains: param -> bool}

    The implementation uses:
      - `list()` to enumerate parameters for most checks, and
      - `contains` specifically for the contragredient-closure check.

  Why this refinement matters
  - `F4FPPBottomLayerAlgGoalsTheory` already provides a list-based skeleton.
    However, the SML dual-closure check is not “MEM in the enumerated list”;
    it is “`contains` returns true” (backed by a hash-table lookup).
  - This theory makes that explicit and isolates the single extra obligation
    we need about the data structure:
        `contains` agrees with membership in the set represented by `list`.

  Status
  - “OK”: definitions and list/set lemmas only.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPBottomLayerGoalsTheory;
open F4FPPBottomLayerAlgGoalsTheory;

val _ = new_theory "F4FPPBottomLayerParamSetGoals";

(* A minimal abstract view of SML’s `param_set`. *)
Type param_set = ``:param list # (param -> bool)``;

Definition ps_list_def:
  ps_list (s:param_set) = FST s
End

Definition ps_contains_def:
  ps_contains (s:param_set) = SND s
End

(* Representation invariant: `contains` agrees with set-membership of `list`. *)
Definition param_set_rep_ok_def:
  param_set_rep_ok (s:param_set) (U:param set) <=>
    (U = set (ps_list s)) /\
    (!p. ps_contains s p <=> p IN U)
End

(* Dual-closure as implemented by `verify_unitary_dual_set`: iterate `list`,
   and test `contains (contragredient p)` for each element. *)
Definition dual_closed_contains_ok_def:
  dual_closed_contains_ok (s:param_set) <=>
    EVERY (\p. ps_contains s (contragredient p)) (ps_list s)
End

Theorem dual_closed_contains_ok_iff_set:
  !s U.
    param_set_rep_ok s U ==>
      (dual_closed_contains_ok s <=> dual_closed U)
Proof
  rw[param_set_rep_ok_def, dual_closed_contains_ok_def, dual_closed_def, ps_list_def, ps_contains_def]
  \\ simp[EVERY_MEM]
QED

(* A combined “bottom-layer ok via param_set interface” predicate. For all
   checks except the dual check, we can reuse the list-level versions from
   `F4FPPBottomLayerAlgGoalsTheory`. *)
Definition bottom_layer_ok_param_set_def:
  bottom_layer_ok_param_set g (dirac:bool) (s:param_set) <=>
    standard_final_list_ok (ps_list s) /\
    lambda_table_list_ok g (ps_list s) /\
    twist_equiv_list_ok (ps_list s) /\
    hermitian_list_ok (ps_list s) /\
    unitary_if_list dirac (ps_list s) /\
    dual_closed_contains_ok s
End

Theorem bottom_layer_ok_param_set_iff_set:
  !g dirac s U.
    param_set_rep_ok s U ==>
      (bottom_layer_ok_param_set g dirac s <=> bottom_layer_ok g dirac U)
Proof
  rpt strip_tac
  \\ fs[param_set_rep_ok_def]
  \\ simp
      [ bottom_layer_ok_param_set_def
      , bottom_layer_ok_def
      , standard_final_list_ok_iff_set
      , lambda_table_list_ok_iff_set
      , twist_equiv_list_ok_iff_set
      , hermitian_list_ok_iff_set
      , unitary_if_list_iff_set
      ]
  \\ `param_set_rep_ok s (set (ps_list s))` by (simp[param_set_rep_ok_def])
  \\ `dual_closed_contains_ok s <=> dual_closed (set (ps_list s))` by
       metis_tac[dual_closed_contains_ok_iff_set]
  \\ simp[]
QED

val _ = export_theory ();
