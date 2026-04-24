(*
  File: formal/hol4/F4FPPBottomLayerGoalsAtlasEqScript.sml

  Purpose
  - Provide modulo-`atlas_eq` variants of the bottom-layer set predicates from
    `F4FPPBottomLayerGoalsTheory`.

  Motivation
  - The concrete SML implementation’s data structures (ParamHash/ParamSet) test
    membership using Atlas semantic equality (`atlas_eq`), not HOL `=`.
  - Many bottom-layer checks are intended to be properties of *semantic*
    parameters, i.e. they should be stable under replacing representatives by
    `atlas_eq`-equivalent ones.

  What this theory adds
  - Predicates `*_atlas_eq` that quantify over `mem_set_atlas_eq p U` rather
    than `p IN U`.
  - “Lifting” lemmas: under `atlas_eq_equiv` + congruence contracts, the
    existing representative-level predicates imply their modulo-`atlas_eq`
    counterparts.

  Status
  - OK: set/list reasoning plus the explicit congruence contracts.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPBottomLayerGoalsTheory;
open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;

val _ = new_theory "F4FPPBottomLayerGoalsAtlasEq";

Definition standard_final_ok_atlas_eq_def:
  standard_final_ok_atlas_eq (U:param set) <=>
    !p. mem_set_atlas_eq p U ==> is_standard p /\ is_final p
End

Definition lambda_table_set_ok_atlas_eq_def:
  lambda_table_set_ok_atlas_eq g (U:param set) <=>
    needs_lambda_table_check g ==> (!p. mem_set_atlas_eq p U ==> lambda_table_ok g p)
End

Definition twist_equiv_ok_atlas_eq_def:
  twist_equiv_ok_atlas_eq (U:param set) <=>
    !p. mem_set_atlas_eq p U ==> param_equiv p (twist p)
End

Definition hermitian_ok_atlas_eq_def:
  hermitian_ok_atlas_eq (U:param set) <=>
    !p. mem_set_atlas_eq p U ==> is_hermitian p
End

Definition unitary_ok_atlas_eq_def:
  unitary_ok_atlas_eq (U:param set) <=>
    !p. mem_set_atlas_eq p U ==> is_unitary p
End

Definition unitary_if_atlas_eq_def:
  unitary_if_atlas_eq (dirac:bool) (U:param set) <=>
    if dirac then unitary_ok_atlas_eq U else T
End

Definition dual_closed_atlas_eq_def:
  dual_closed_atlas_eq (U:param set) <=>
    !p. mem_set_atlas_eq p U ==> mem_set_atlas_eq (contragredient p) U
End

Definition bottom_layer_ok_atlas_eq_def:
  bottom_layer_ok_atlas_eq g (dirac:bool) (U:param set) <=>
    standard_final_ok_atlas_eq U /\
    lambda_table_set_ok_atlas_eq g U /\
    twist_equiv_ok_atlas_eq U /\
    hermitian_ok_atlas_eq U /\
    unitary_if_atlas_eq dirac U /\
    dual_closed_atlas_eq U
End

Definition bottom_layer_total_ok_atlas_eq_def:
  bottom_layer_total_ok_atlas_eq g (dirac:bool) (U:param set) <=>
    if group_is_compact g then
      (!p. p IN rho_set g ==> mem_set_atlas_eq p U)
    else
      bottom_layer_ok_atlas_eq g dirac U
End

(* A generic “lifting” lemma: a unary predicate stable under `atlas_eq` can be
   extended from representatives `q IN U` to all `p` in the `atlas_eq`-closure. *)
Theorem atlas_eq_congruent_pred_lift:
  !P U.
    atlas_eq_equiv /\
    (!p q. atlas_eq p q ==> (P p <=> P q)) /\
    (!q. q IN U ==> P q) ==>
      (!p. mem_set_atlas_eq p U ==> P p)
Proof
  rpt gen_tac
  \\ strip_tac
  \\ gen_tac
  \\ strip_tac
  \\ fs[mem_set_atlas_eq_def]
  \\ res_tac
  \\ `P p <=> P q` by metis_tac[]
  \\ metis_tac[]
QED

Theorem standard_final_ok_imp_standard_final_ok_atlas_eq:
  !U.
    atlas_eq_equiv /\ atlas_eq_congruent_bottom_layer /\ standard_final_ok U ==>
      standard_final_ok_atlas_eq U
Proof
  rpt gen_tac
  \\ strip_tac
  \\ rw[standard_final_ok_atlas_eq_def, mem_set_atlas_eq_def]
  \\ fs[standard_final_ok_def]
  \\ first_x_assum drule
  \\ strip_tac
  \\ fs[atlas_eq_congruent_bottom_layer_def]
  \\ metis_tac[]
QED

Theorem hermitian_ok_imp_hermitian_ok_atlas_eq:
  !U.
    atlas_eq_equiv /\ atlas_eq_congruent_bottom_layer /\ hermitian_ok U ==>
      hermitian_ok_atlas_eq U
Proof
  rpt gen_tac
  \\ strip_tac
  \\ rw[hermitian_ok_atlas_eq_def, mem_set_atlas_eq_def]
  \\ fs[hermitian_ok_def]
  \\ first_x_assum drule
  \\ strip_tac
  \\ fs[atlas_eq_congruent_bottom_layer_def]
  \\ metis_tac[]
QED

Theorem unitary_ok_imp_unitary_ok_atlas_eq:
  !U.
    atlas_eq_equiv /\ atlas_eq_congruent_bottom_layer /\ unitary_ok U ==>
      unitary_ok_atlas_eq U
Proof
  rpt gen_tac
  \\ strip_tac
  \\ rw[unitary_ok_atlas_eq_def, mem_set_atlas_eq_def]
  \\ fs[unitary_ok_def]
  \\ first_x_assum drule
  \\ strip_tac
  \\ fs[atlas_eq_congruent_bottom_layer_def]
  \\ metis_tac[]
QED

Theorem unitary_if_imp_unitary_if_atlas_eq:
  !dirac U.
    atlas_eq_equiv /\ atlas_eq_congruent_bottom_layer /\ unitary_if dirac U ==>
      unitary_if_atlas_eq dirac U
Proof
  rpt gen_tac
  \\ strip_tac
  \\ Cases_on `dirac`
  \\ fs[unitary_if_def, unitary_if_atlas_eq_def]
  \\ metis_tac[unitary_ok_imp_unitary_ok_atlas_eq]
QED

Theorem lambda_table_set_ok_imp_lambda_table_set_ok_atlas_eq:
  !g U.
    atlas_eq_equiv /\ atlas_eq_congruent_bottom_layer /\ lambda_table_set_ok g U ==>
      lambda_table_set_ok_atlas_eq g U
Proof
  rpt gen_tac
  \\ strip_tac
  \\ fs[]
  \\ rw[lambda_table_set_ok_atlas_eq_def, mem_set_atlas_eq_def]
  \\ `lambda_table_ok g q` by metis_tac[lambda_table_set_ok_def]
  \\ fs[atlas_eq_congruent_bottom_layer_def]
  \\ metis_tac[]
QED

Theorem twist_equiv_ok_imp_twist_equiv_ok_atlas_eq:
  !U.
    atlas_eq_equiv /\ atlas_eq_congruent_bottom_layer /\ twist_equiv_ok U ==>
      twist_equiv_ok_atlas_eq U
Proof
  rpt gen_tac
  \\ strip_tac
  \\ fs[]
  \\ rw[twist_equiv_ok_atlas_eq_def, mem_set_atlas_eq_def]
  \\ fs[atlas_eq_congruent_bottom_layer_def]
  \\ metis_tac[twist_equiv_ok_def]
QED

Theorem dual_closed_imp_dual_closed_atlas_eq:
  !U.
    atlas_eq_equiv /\ atlas_eq_congruent_bottom_layer /\ dual_closed U ==>
      dual_closed_atlas_eq U
Proof
  rw[dual_closed_def, dual_closed_atlas_eq_def, atlas_eq_equiv_def, mem_set_atlas_eq_def]
  \\ first_x_assum drule
  \\ strip_tac
  \\ qexists_tac `contragredient q`
  \\ conj_tac >- metis_tac[]
  \\ fs[atlas_eq_congruent_bottom_layer_def]
  \\ metis_tac[]
QED

Theorem bottom_layer_ok_imp_bottom_layer_ok_atlas_eq:
  !g dirac U.
    atlas_eq_equiv /\ atlas_eq_congruent_bottom_layer /\ bottom_layer_ok g dirac U ==>
      bottom_layer_ok_atlas_eq g dirac U
Proof
  rw[bottom_layer_ok_def, bottom_layer_ok_atlas_eq_def]
  \\ metis_tac
      [ standard_final_ok_imp_standard_final_ok_atlas_eq
      , lambda_table_set_ok_imp_lambda_table_set_ok_atlas_eq
      , twist_equiv_ok_imp_twist_equiv_ok_atlas_eq
      , hermitian_ok_imp_hermitian_ok_atlas_eq
      , unitary_if_imp_unitary_if_atlas_eq
      , dual_closed_imp_dual_closed_atlas_eq
      ]
QED

Theorem bottom_layer_total_ok_imp_bottom_layer_total_ok_atlas_eq:
  !g dirac U.
    atlas_eq_equiv /\ atlas_eq_congruent_bottom_layer /\ bottom_layer_total_ok g dirac U ==>
      bottom_layer_total_ok_atlas_eq g dirac U
Proof
  rw[bottom_layer_total_ok_def, bottom_layer_total_ok_atlas_eq_def]
  >- (
    rw[mem_set_atlas_eq_def]
    \\ qexists_tac `p`
    \\ conj_tac
    >- fs[SUBSET_DEF]
    \\ fs[atlas_eq_equiv_def])
  \\ metis_tac[bottom_layer_ok_imp_bottom_layer_ok_atlas_eq]
QED

val _ = export_theory ();
