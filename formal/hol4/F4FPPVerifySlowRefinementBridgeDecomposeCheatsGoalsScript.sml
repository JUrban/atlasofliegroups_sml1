(*
  File: formal/hol4/F4FPPVerifySlowRefinementBridgeDecomposeCheatsGoalsScript.sml

  Purpose
  - Further refine the slow-side “domain refinement” bridge by isolating the
    two *smaller* obligations that are sufficient to derive `slow_refinement_ok`:
      - correctness of the component enumerator lists, and
      - agreement between the slow program’s nested-loop enumeration and the
        canonical product list.

  Why this file exists
  - `F4FPPVerifySlowBridgeDetailedCheatsGoalsTheory` previously recorded a
    single cheated lemma:
      `slow_program_succeeds g ⇒ slow_refinement_ok g`.
  - But the intended proof structure is already available as an OK composition
    lemma in `F4FPPVerifySlowRefinementBridgeDecomposeGoalsTheory`:
      `slow_component_lists_ok ∧ slow_dom_list_agrees_components ⇒ slow_refinement_ok`.
  - This file records the two smaller “program success ⇒ obligation” bridge
    lemmas, leaving the recombination to be handled without additional `cheat`.

  Status
  - The lemmas here are currently `cheat`ed.
  - Their statements are meant to be the stable interface for future CakeML
    evaluation proofs and/or shallow HOL models of the slow program’s
    enumerator functions (`loadBarycenters`, `loadLambdasByX`, `for_domain`).
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifySMLBridgeGoalsTheory;
open F4FPPVerifySlowRefinementBridgeDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifySlowRefinementBridgeDecomposeCheatsGoals";

Theorem slow_program_succeeds_imp_slow_component_lists_ok:
  !g. slow_program_succeeds g ==> slow_component_lists_ok g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - show `SimplerVerifyF4FPP.loadBarycenters` returns a list satisfying
      `AllBarycenters_list_correct g`,
    - show `SimplerVerifyF4FPP.loadLambdasByX` returns per-x lists satisfying
      `FPP_lambdas_list_correct g`,
    - show the KGB enumeration used is `KGB_list_correct g`.
  *)
  cheat
QED

Theorem slow_program_succeeds_imp_slow_dom_list_agrees_components:
  !g. slow_program_succeeds g ==> slow_dom_list_agrees_components g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - relate the slow program’s nested-loop enumeration (captured abstractly by
      `slow_domain_list`) to the canonical product list
      `dom_list_from_components`.
    - this typically splits into:
        (1) `dom_list_is_slow_domain_list g`, and
        (2) `slow_domain_list_is_components g`.
  *)
  cheat
QED

val _ = export_theory ();

