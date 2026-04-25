(*
  File: formal/hol4/F4FPPVerifyRefinedBridgeCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) refined bridge obligations that connect
    `fast_program_succeeds` / `slow_program_succeeds` to the refined obligation
    bundles used by `F4FPPVerifyRefinedMainGoalsTheory`.

  Rationale
  - `F4FPPVerifyRefinedBridgeGoalsTheory` should remain “OK” composition-only,
    chaining named obligation bundles to the main equivalence statement.
  - These bridge lemmas are the intended attachment points for CakeML proofs
    about program evaluation/control-flow and for Atlas/FFI contracts.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifySlowRefineGoalsTheory;
open F4FPPVerifyFastRefineGoalsTheory;
open F4FPPVerifyFastRefineAtlasEqGoalsTheory;
open F4FPPBottomLayerGoalsTheory;
open F4FPPVerifyRefinedMainGoalsTheory;
open F4FPPVerifySMLBridgeGoalsTheory;

val _ = new_theory "F4FPPVerifyRefinedBridgeCheatsGoals";

Theorem fast_program_succeeds_imp_refined_fast_obligations:
  !g dirac.
    fast_program_succeeds g dirac ==>
      fast_semantic_ok g /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - connect `VerifyF4FPP.sml` to the fast compute bridges and bottom-layer
      bridges, then use the (OK) refinement stack to obtain `fast_semantic_ok`
      and `bottom_layer_total_ok`.
  *)
  cheat
QED

Theorem fast_program_succeeds_imp_refined_fast_obligations_atlas_eq:
  !g dirac.
    fast_program_succeeds g dirac ==>
      fast_semantic_ok_atlas_eq g /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - as above, but routing through the modulo-`atlas_eq` fast witness bundle.
  *)
  cheat
QED

Theorem slow_program_succeeds_imp_refined_slow_obligations:
  !g.
    slow_program_succeeds g ==>
      slow_refinement_ok g /\
      slow_ok_components g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - connect `SimplerVerifyF4FPP.sml` to the slow domain/missing decomposition
      bridges, then use the (OK) refinement stack to obtain `slow_refinement_ok`
      and `slow_ok_components`.
  *)
  cheat
QED

val _ = export_theory ();

