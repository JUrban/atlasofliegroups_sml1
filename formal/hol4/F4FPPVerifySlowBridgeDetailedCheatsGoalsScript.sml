(*
  File: formal/hol4/F4FPPVerifySlowBridgeDetailedCheatsGoalsScript.sml

  Purpose
  - Isolate the remaining (currently `cheat`ed) slow-side bridge lemma that
    connects `slow_program_succeeds g` to the refinement bundle
    `slow_refinement_ok g` (domain enumeration correctness).

  Rationale
  - `F4FPPVerifySlowBridgeDetailedGoalsTheory` should be “OK” composition-only,
    assembling refined slow obligations from smaller bridge lemmas.
  - This particular lemma is about the concrete program’s enumerators and I/O
    behavior and is an intended attachment point for CakeML proofs and/or FFI
    specifications.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifySMLBridgeGoalsTheory;
open F4FPPVerifySlowRefineGoalsTheory;
open F4FPPVerifySlowRefineAtlasEqGoalsTheory;
open F4FPPVerifySlowRefinementBridgeDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifySlowBridgeDetailedCheatsGoals";

Theorem slow_program_succeeds_imp_slow_refinement_ok:
  !g. slow_program_succeeds g ==> slow_refinement_ok g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - show `loadBarycenters` enumerates `AllBarycenters g` correctly
    - show `loadLambdasByX` enumerates `FPP_lambdas g x` correctly for each x
    - show the nested `for_domain` loop corresponds to `dom_list_from_components`
      (hence `dom_list_is_components` with an appropriate choice of `dom_list`)

    For a more decomposed route, prove (from `slow_program_succeeds g`):
      - `slow_component_lists_ok g`
      - `slow_dom_list_agrees_components g`
    and then apply
      `slow_component_lists_and_dom_list_agreement_imp_slow_refinement_ok`.
  *)
  cheat
QED

val _ = export_theory ();

