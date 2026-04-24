(*
  File: formal/hol4/F4FPPVerifySlowRefinementBridgeDecomposeGoalsScript.sml

  Purpose
  - Decompose the slow-side refinement bundle `slow_refinement_ok g` (used by
    `F4FPPVerifyRefinedMainGoalsTheory`) into smaller obligations that align
    with the concrete SML structure in `SimplerVerifyF4FPP.sml`.

  Recall
  - `slow_refinement_ok g` currently packages:
      - component enumerator correctness
          (`KGB_list_correct`, `FPP_lambdas_list_correct`, `AllBarycenters_list_correct`)
      - and the agreement `dom_list_is_components g`
        (i.e. the program’s `dom_list g` is the canonical product list).

  But the SML slow program naturally presents a *domain enumerator loop*
  (represented abstractly here by `slow_domain_list g` in
  `F4FPPVerifySlowProgramDecomposeBridgeGoalsTheory`) rather than directly
  exposing `dom_list g`.

  This theory introduces an explicit intermediate obligation:
  - `dom_list_is_slow_domain_list g`:
      the abstract goal-layer `dom_list g` equals the slow loop’s list
      `slow_domain_list g`.

  Then, together with `slow_domain_list_is_components g`, we can derive
  `dom_list_is_components g` and hence `slow_refinement_ok g`.

  Status
  - The decomposition and composition lemmas are “OK”.
  - Any bridge from concrete SML execution to these obligations remains
    assumption-heavy and is expected to be discharged later.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyDomainGoalsTheory;
open F4FPPVerifyComponentsBridgeGoalsTheory;
open F4FPPVerifyRefinedMainGoalsTheory;
open F4FPPVerifySlowProgramDecomposeBridgeGoalsTheory;

val _ = new_theory "F4FPPVerifySlowRefinementBridgeDecomposeGoals";

Definition dom_list_is_slow_domain_list_def:
  dom_list_is_slow_domain_list g <=>
    dom_list g = slow_domain_list g
End

Definition slow_component_lists_ok_def:
  slow_component_lists_ok g <=>
    KGB_list_correct g /\
    FPP_lambdas_list_correct g /\
    AllBarycenters_list_correct g
End

Definition slow_dom_list_agrees_components_def:
  slow_dom_list_agrees_components g <=>
    dom_list_is_slow_domain_list g /\
    slow_domain_list_is_components g
End

Theorem slow_dom_list_agrees_components_imp_dom_list_is_components:
  !g.
    slow_dom_list_agrees_components g ==>
      dom_list_is_components g
Proof
  rw[slow_dom_list_agrees_components_def,
     dom_list_is_slow_domain_list_def,
     slow_domain_list_is_components_def,
     dom_list_is_components_def]
QED

Theorem slow_component_lists_and_dom_list_agreement_imp_slow_refinement_ok:
  !g.
    slow_component_lists_ok g /\
    slow_dom_list_agrees_components g ==>
      slow_refinement_ok g
Proof
  rw[slow_component_lists_ok_def, slow_refinement_ok_def]
  \\ metis_tac[slow_dom_list_agrees_components_imp_dom_list_is_components]
QED

val _ = export_theory ();

