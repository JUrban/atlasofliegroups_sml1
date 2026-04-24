(*
  File: formal/hol4/F4FPPVerifySlowRefineAtlasEqGoalsScript.sml

  Purpose
  - Provide an “OK” refinement lemma for the slow checker that ties the
    list-based slow-fold algorithm `check_domain_fun_atlas_eq` to the abstract
    completeness predicate `complete_rel_atlas_eq` (from
    `F4FPPVerifySpecAtlasEqGoalsTheory`).

  Why this matters
  - The real slow SML script checks “membership in the fast set” using the
    Atlas C++ equality (`atlas_param_equal`) via ParamHash, not HOL `=`.
  - This file isolates that difference by keeping the slow checker’s logical
    contract modulo `atlas_eq`.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyAlgAtlasEqGoalsTheory;
open F4FPPVerifySpecAtlasEqGoalsTheory;
open F4FPPVerifyDomainGoalsTheory;

val _ = new_theory "F4FPPVerifySlowRefineAtlasEqGoals";

(* Concrete (component-based) notion of the slow checker “returning OK”,
   interpreted modulo `atlas_eq`. *)
Definition slow_ok_components_atlas_eq_def:
  slow_ok_components_atlas_eq g <=>
    (check_domain_fun_atlas_eq g (U_fast g) (dom_list_from_components g) = 0)
End

Theorem slow_ok_components_atlas_eq_imp_complete_rel_atlas_eq:
  !g.
    KGB_list_correct g /\
    FPP_lambdas_list_correct g /\
    AllBarycenters_list_correct g /\
    slow_ok_components_atlas_eq g ==>
      complete_rel_atlas_eq g (D_slow g) (U_fast g)
Proof
  rw[slow_ok_components_atlas_eq_def]
  \\ `complete_rel_atlas_eq g (set (dom_list_from_components g)) (U_fast g)` by
       metis_tac[check_domain_fun_atlas_eq_eq0_imp_complete_rel_atlas_eq_list]
  \\ `set (dom_list_from_components g) = D_slow g` by
       metis_tac[dom_list_from_components_correct]
  \\ fs[]
QED

val _ = export_theory ();

