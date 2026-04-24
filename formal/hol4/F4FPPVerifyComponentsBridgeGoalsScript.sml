(*
  File: formal/hol4/F4FPPVerifyComponentsBridgeGoalsScript.sml

  Purpose
  - Provide an “OK” glue layer that connects the *component-based* domain
    enumeration from `F4FPPVerifyDomainGoalsTheory` to the abstract
    program-output constants from `F4FPPVerifyGoalsTheory`.

  Why this exists
  - `F4FPPVerifyGoalsTheory` talks about an abstract list `dom_list g` that is
    meant to represent the slow program’s enumeration.
  - `F4FPPVerifyDomainGoalsTheory` provides a canonical product construction
    `dom_list_from_components g` built from component lists.
  - This theory isolates the refinement step “the slow program’s `dom_list`
    equals the component-product list”, and shows how that implies:
      - `dom_list_correct g`
      - equivalence between `slow_ok` and `slow_ok_components`

  This keeps the main equivalence theorem stable while allowing the slow side
  to be refined in stages.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPVerifyDomainGoalsTheory;
open F4FPPVerifySlowRefineGoalsTheory;

val _ = new_theory "F4FPPVerifyComponentsBridgeGoals";

(* Refinement assumption: the abstract slow program list is the canonical
   component-product list. *)
Definition dom_list_is_components_def:
  dom_list_is_components g <=>
    dom_list g = dom_list_from_components g
End

Theorem dom_list_is_components_imp_dom_list_correct:
  !g.
    dom_list_is_components g /\
    KGB_list_correct g /\
    FPP_lambdas_list_correct g /\
    AllBarycenters_list_correct g ==>
      dom_list_correct g
Proof
  rw[dom_list_is_components_def, dom_list_correct_def, Dom_def]
  \\ fs[dom_list_from_components_correct]
QED

Theorem dom_list_is_components_imp_slow_ok_iff:
  !g.
    dom_list_is_components g ==>
      (slow_ok g <=> slow_ok_components g)
Proof
  rw[dom_list_is_components_def, slow_ok_def, slow_ok_components_def, U_fast_def]
QED

val _ = export_theory ();

