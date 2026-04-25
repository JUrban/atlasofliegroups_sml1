(*
  File: formal/hol4/F4FPPVerifyFastComputeBridgeCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) execution/FFI bridge theorems for the fast
    compute phase, so that `F4FPPVerifyFastComputeBridgeGoalsTheory` can remain
    entirely “OK” (definitions + logical consequences only).

  What is “cheated” here
  - These theorems state that the SML compute phase ran successfully and
    therefore established the abstract compute-phase obligation bundles:
      - `fast_compute_obligations g`, and
      - `fast_compute_obligations_atlas_eq g`.

  Intended discharge plan (later)
  - Replace each `cheat` by a proof that connects:
      (1) the control-flow of the SML compute phase,
      (2) the ParamHash state invariant / representation facts, and
      (3) the semantic “witness” argument about which triples are enumerated
          (`fast_considers`) and which params are inserted.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyFastComputeBridgeDecomposeCheatsGoalsTheory;

val _ = new_theory "F4FPPVerifyFastComputeBridgeCheatsGoals";

Theorem fast_compute_program_succeeds_imp_obligations:
  !g. fast_compute_program_succeeds g ==> fast_compute_obligations g
Proof
  (* Derived: the actual `cheat` surface is the smaller bridge obligations
     recorded in `F4FPPVerifyFastComputeBridgeDecomposeCheatsGoalsTheory`. *)
  metis_tac[fast_compute_program_succeeds_imp_fast_compute_obligations_factored]
QED

Theorem fast_compute_program_succeeds_imp_obligations_atlas_eq:
  !g. fast_compute_program_succeeds g ==> fast_compute_obligations_atlas_eq g
Proof
  cheat
QED

val _ = export_theory ();
