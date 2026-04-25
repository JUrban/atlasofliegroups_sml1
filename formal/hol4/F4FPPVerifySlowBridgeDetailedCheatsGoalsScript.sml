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
open F4FPPVerifySlowRefinementBridgeDecomposeCheatsGoalsTheory;

val _ = new_theory "F4FPPVerifySlowBridgeDetailedCheatsGoals";

Theorem slow_program_succeeds_imp_slow_refinement_ok:
  !g. slow_program_succeeds g ==> slow_refinement_ok g
Proof
  rpt strip_tac
  \\ match_mp_tac slow_component_lists_and_dom_list_agreement_imp_slow_refinement_ok
  \\ metis_tac
      [ slow_program_succeeds_imp_slow_component_lists_ok
      , slow_program_succeeds_imp_slow_dom_list_agrees_components
      ]
QED

val _ = export_theory ();
