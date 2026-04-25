(*
  File: formal/hol4/F4FPPBottomLayerParamSetAtlasEqGoalsScript.sml

  Purpose
  - Provide the “param_set interface” refinement layer for the bottom-layer
    checks in the realistic setting where membership is tested modulo Atlas
    semantic equality `atlas_eq` (i.e. via `mem_set_atlas_eq`), not HOL `=`.

  Background
  - `F4FPPBottomLayerParamSetGoalsTheory` models the SML record
      { list : unit -> param list, contains : param -> bool }
    as a pair `(ps_list s, ps_contains s)` and states a representation invariant
    `param_set_rep_ok` where `contains p` is equivalent to `p IN U` for the set
    `U = set (list())`.
  - The real SML fast data structures (ParamHash/ParamSet) use Atlas equality
    (`atlas_param_equal`) to implement membership. In HOL4 this is modeled by
    `mem_set_atlas_eq p U`, meaning “`p` is `atlas_eq` to some representative in
    `U`”.

  What this theory adds
  - A modulo-`atlas_eq` representation invariant `param_set_rep_ok_atlas_eq`.
  - A lemma showing the SML-style dual-closure check (iterate `list`, query
    `contains (contragredient p)`) implies the modulo-`atlas_eq` set predicate
    `dual_closed_atlas_eq`.
  - A compositional lemma showing the combined param_set-based bottom-layer
    predicate implies `bottom_layer_ok_atlas_eq`.

  Status
  - “OK”: pure list/set reasoning plus the explicit `atlas_eq` congruence
    contracts from `F4FPPVerifyAtlasFFIContractsGoalsTheory`.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPBottomLayerGoalsTheory;
open F4FPPBottomLayerAlgGoalsTheory;
open F4FPPBottomLayerParamSetGoalsTheory;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;
open F4FPPBottomLayerGoalsAtlasEqTheory;

val _ = new_theory "F4FPPBottomLayerParamSetAtlasEqGoals";

(* Representation invariant: `contains` agrees with modulo-`atlas_eq` membership
   in the set represented by `list`. This matches ParamHash/ParamSet behaviour. *)
Definition param_set_rep_ok_atlas_eq_def:
  param_set_rep_ok_atlas_eq (s:param_set) (U:param set) <=>
    (U = set (ps_list s)) /\
    (!p. ps_contains s p <=> mem_set_atlas_eq p U)
End

(* The SML dual-closure check is phrased in terms of `contains`. Under the
   modulo-`atlas_eq` representation invariant, it implies the set-level
   predicate `dual_closed_atlas_eq`. *)
Theorem dual_closed_contains_ok_imp_dual_closed_atlas_eq:
  !s U.
    atlas_eq_equiv /\
    atlas_eq_congruent_bottom_layer /\
    param_set_rep_ok_atlas_eq s U /\
    dual_closed_contains_ok s ==>
      dual_closed_atlas_eq U
Proof
  rpt gen_tac
  \\ strip_tac
  \\ fs[]
  \\ qpat_x_assum `param_set_rep_ok_atlas_eq s U`
       (strip_assume_tac o REWRITE_RULE[param_set_rep_ok_atlas_eq_def])
  \\ rw[dual_closed_atlas_eq_def]
  \\ qpat_x_assum `mem_set_atlas_eq p U`
       (qx_choose_then `q` strip_assume_tac o REWRITE_RULE[mem_set_atlas_eq_def])
  \\ `q IN set (ps_list s)` by metis_tac[]
  \\ `MEM q (ps_list s)` by fs[]
  \\ `ps_contains s (contragredient q)` by fs[dual_closed_contains_ok_def, EVERY_MEM]
  \\ qpat_x_assum `!p. ps_contains s p <=> mem_set_atlas_eq p U`
       (qspec_then `contragredient q` (fn th => imp_res_tac (fst (EQ_IMP_RULE th))))
  \\ qpat_x_assum `mem_set_atlas_eq (contragredient q) U`
       (qx_choose_then `r` strip_assume_tac o REWRITE_RULE[mem_set_atlas_eq_def])
  \\ rw[mem_set_atlas_eq_def]
  \\ qexists_tac `r`
  \\ conj_tac >- metis_tac[]
  \\ fs[atlas_eq_equiv_def, atlas_eq_congruent_bottom_layer_def]
  \\ metis_tac[]
QED

(* Combined param_set-based predicate implies the modulo-`atlas_eq` bottom-layer
   set predicate. *)
Theorem bottom_layer_ok_param_set_imp_bottom_layer_ok_atlas_eq:
  !g dirac s U.
    atlas_eq_equiv /\
    atlas_eq_congruent_bottom_layer /\
    param_set_rep_ok_atlas_eq s U /\
    bottom_layer_ok_param_set g dirac s ==>
      bottom_layer_ok_atlas_eq g dirac U
Proof
  rpt gen_tac
  \\ strip_tac
  \\ fs[param_set_rep_ok_atlas_eq_def, bottom_layer_ok_param_set_def]
  \\ rw[bottom_layer_ok_atlas_eq_def]
  >- metis_tac[standard_final_list_ok_iff_set, standard_final_ok_imp_standard_final_ok_atlas_eq]
  >- metis_tac[lambda_table_list_ok_iff_set, lambda_table_set_ok_imp_lambda_table_set_ok_atlas_eq]
  >- metis_tac[twist_equiv_list_ok_iff_set, twist_equiv_ok_imp_twist_equiv_ok_atlas_eq]
  >- metis_tac[hermitian_list_ok_iff_set, hermitian_ok_imp_hermitian_ok_atlas_eq]
  >- metis_tac[unitary_if_list_iff_set, unitary_if_imp_unitary_if_atlas_eq]
  >- metis_tac[dual_closed_contains_ok_imp_dual_closed_atlas_eq, param_set_rep_ok_atlas_eq_def]
QED

(* Total bottom-layer postcondition phrased via the `param_set` interface, using
   `contains` for the compact-group seeding obligation. *)
Definition bottom_layer_total_ok_param_set_atlas_eq_def:
  bottom_layer_total_ok_param_set_atlas_eq g (dirac:bool) (s:param_set) <=>
    if group_is_compact g then
      (!p. p IN rho_set g ==> ps_contains s p)
    else
      bottom_layer_ok_param_set g dirac s
End

Theorem bottom_layer_total_ok_param_set_atlas_eq_imp_bottom_layer_total_ok_atlas_eq:
  !g dirac s U.
    atlas_eq_equiv /\
    atlas_eq_congruent_bottom_layer /\
    param_set_rep_ok_atlas_eq s U /\
    bottom_layer_total_ok_param_set_atlas_eq g dirac s ==>
      bottom_layer_total_ok_atlas_eq g dirac U
Proof
  rpt gen_tac
  \\ strip_tac
  \\ Cases_on `group_is_compact g`
  \\ fs[bottom_layer_total_ok_param_set_atlas_eq_def, bottom_layer_total_ok_atlas_eq_def]
  >- (
    rw[]
    \\ fs[param_set_rep_ok_atlas_eq_def]
    \\ metis_tac[])
  \\ irule bottom_layer_ok_param_set_imp_bottom_layer_ok_atlas_eq
  \\ metis_tac[]
QED

val _ = export_theory ();
