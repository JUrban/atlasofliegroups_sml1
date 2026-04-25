(*
  File: formal/hol4/F4FPPVerifyFastProgramSplitBridgeCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bridge lemma that connects the abstract
    fast “whole program succeeded” predicate `fast_program_succeeds` to the
    corresponding per-phase success predicates:
      - `fast_compute_program_succeeds`
      - `bottom_layer_program_succeeds`

  Rationale
  - `F4FPPVerifyFastProgramSplitBridgeGoalsTheory` is intended to remain “OK”
    (composition-only): it should not contain `cheat`.
  - This split lemma is a control-flow fact about the concrete SML program
    `atlas-scripts-sml/VerifyF4FPP.sml` and will ultimately be discharged by a
    CakeML evaluation proof (or an agreed shallow semantics model).
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifySMLBridgeGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyGlobalDiracBridgeGoalsTheory;

val _ = new_theory "F4FPPVerifyFastProgramSplitBridgeCheatsGoals";

Theorem fast_program_succeeds_imp_phase_success:
  !g dirac.
    fast_program_succeeds g dirac ==>
      fast_compute_program_succeeds g /\ bottom_layer_program_succeeds g dirac
Proof
  (*
    Intended proof (later, without `cheat`):
    - unfold `VerifyF4FPP.compute` and show it calls the two phases in sequence,
      and “overall success” implies each returns without raising.
  *)
  cheat
QED

val _ = export_theory ();

