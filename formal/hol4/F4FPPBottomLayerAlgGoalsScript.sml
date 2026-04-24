(*
  File: formal/hol4/F4FPPBottomLayerAlgGoalsScript.sml

  Purpose
  - Provide an “algorithmic skeleton” for the bottom-layer verification checks
    performed by `atlas-scripts-sml/FPP_globalDirac.sml`.

  Why this exists
  - `F4FPPBottomLayerGoalsTheory` specifies the bottom-layer checks as
    *set-level* predicates over a set `U` of parameters.
  - The SML implementation operates by:
      - enumerating a list `ps` of parameters from a data structure, and
      - checking that no element violates each predicate, typically via
        `List.filter`/`null` or by per-element membership checks.
  - This theory defines list-level predicates matching that control-flow and
    proves they are equivalent to the set-level specs when `U = set ps`.

  Status
  - “OK”: pure list/set reasoning over abstract primitives from
    `F4FPPBottomLayerGoalsTheory`. No connection to Poly/ML evaluation or Atlas
    FFI is attempted here.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPBottomLayerGoalsTheory;

val _ = new_theory "F4FPPBottomLayerAlgGoals";

(* --- List-level check predicates (intended to match SML control-flow) --- *)

Definition standard_final_list_ok_def:
  standard_final_list_ok (ps:param list) <=>
    EVERY (\p. is_standard p /\ is_final p) ps
End

Definition lambda_table_list_ok_def:
  lambda_table_list_ok g (ps:param list) <=>
    (needs_lambda_table_check g ==> EVERY (lambda_table_ok g) ps)
End

Definition twist_equiv_list_ok_def:
  twist_equiv_list_ok (ps:param list) <=>
    EVERY (\p. param_equiv p (twist p)) ps
End

Definition hermitian_list_ok_def:
  hermitian_list_ok (ps:param list) <=>
    EVERY is_hermitian ps
End

Definition unitary_list_ok_def:
  unitary_list_ok (ps:param list) <=>
    EVERY is_unitary ps
End

Definition unitary_if_list_def:
  unitary_if_list (dirac:bool) (ps:param list) <=>
    if dirac then unitary_list_ok ps else T
End

Definition dual_closed_list_ok_def:
  dual_closed_list_ok (ps:param list) <=>
    EVERY (\p. MEM (contragredient p) ps) ps
End

Definition bottom_layer_ok_list_def:
  bottom_layer_ok_list g (dirac:bool) (ps:param list) <=>
    standard_final_list_ok ps /\
    lambda_table_list_ok g ps /\
    twist_equiv_list_ok ps /\
    hermitian_list_ok ps /\
    unitary_if_list dirac ps /\
    dual_closed_list_ok ps
End

(* --- Equivalence to the set-level predicates --- *)

Theorem standard_final_list_ok_iff_set:
  !ps. standard_final_list_ok ps <=> standard_final_ok (set ps)
Proof
  simp[standard_final_list_ok_def, standard_final_ok_def, EVERY_MEM]
QED

Theorem lambda_table_list_ok_iff_set:
  !g ps. lambda_table_list_ok g ps <=> lambda_table_set_ok g (set ps)
Proof
  simp[lambda_table_list_ok_def, lambda_table_set_ok_def, EVERY_MEM]
QED

Theorem twist_equiv_list_ok_iff_set:
  !ps. twist_equiv_list_ok ps <=> twist_equiv_ok (set ps)
Proof
  simp[twist_equiv_list_ok_def, twist_equiv_ok_def, EVERY_MEM]
QED

Theorem hermitian_list_ok_iff_set:
  !ps. hermitian_list_ok ps <=> hermitian_ok (set ps)
Proof
  simp[hermitian_list_ok_def, hermitian_ok_def, EVERY_MEM]
QED

Theorem unitary_if_list_iff_set:
  !dirac ps. unitary_if_list dirac ps <=> unitary_if dirac (set ps)
Proof
  simp[unitary_if_list_def, unitary_if_def, unitary_list_ok_def, unitary_ok_def, EVERY_MEM]
QED

Theorem dual_closed_list_ok_iff_set:
  !ps. dual_closed_list_ok ps <=> dual_closed (set ps)
Proof
  simp[dual_closed_list_ok_def, dual_closed_def, EVERY_MEM]
QED

Theorem bottom_layer_ok_list_iff_set:
  !g dirac ps. bottom_layer_ok_list g dirac ps <=> bottom_layer_ok g dirac (set ps)
Proof
  simp[bottom_layer_ok_list_def, bottom_layer_ok_def,
       standard_final_list_ok_iff_set,
       lambda_table_list_ok_iff_set,
       twist_equiv_list_ok_iff_set,
       hermitian_list_ok_iff_set,
       unitary_if_list_iff_set,
       dual_closed_list_ok_iff_set]
QED

val _ = export_theory ();

