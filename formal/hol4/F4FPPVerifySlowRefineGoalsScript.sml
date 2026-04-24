(*
  File: formal/hol4/F4FPPVerifySlowRefineGoalsScript.sml

  Purpose
  - Provide an “OK” refinement lemma for the slow checker that ties the
    abstract completeness predicate `complete_rel` to a *concrete list-based*
    domain enumeration built from component lists.

  Key idea
  - The slow script (and its SML port) is fundamentally an iteration over a
    product domain `(x,lambda,gamma)`.
  - `F4FPPVerifyDomainGoalsTheory` defines a canonical list construction
    `dom_list_from_components g` and proves (under component-correctness)
    that it represents exactly `D_slow g` as a set.
  - `F4FPPVerifyAlgTheory` proves that `check_domain_fun = 0` on a list implies
    the set-level predicate `complete_rel` for the corresponding set.

  This theory composes those results to produce a convenient lemma that can be
  used as the slow side of the main set-equality argument.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyAlgTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyDomainGoalsTheory;

val _ = new_theory "F4FPPVerifySlowRefineGoals";

(* Concrete (component-based) notion of the slow checker “returning OK”. *)
Definition slow_ok_components_def:
  slow_ok_components g <=>
    (check_domain_fun g (U_fast g) (dom_list_from_components g) = 0)
End

Theorem slow_ok_components_imp_complete_rel:
  !g.
    KGB_list_correct g /\
    FPP_lambdas_list_correct g /\
    AllBarycenters_list_correct g /\
    slow_ok_components g ==>
      complete_rel g (D_slow g) (U_fast g)
Proof
  rw[slow_ok_components_def]
  \\ `complete_rel g (set (dom_list_from_components g)) (U_fast g)` by
       metis_tac[check_domain_fun_eq0_imp_complete_rel_list]
  \\ `set (dom_list_from_components g) = D_slow g` by metis_tac[dom_list_from_components_correct]
  \\ fs[]
QED

val _ = export_theory ();
