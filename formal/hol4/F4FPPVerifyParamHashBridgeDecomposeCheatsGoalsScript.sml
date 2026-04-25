(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeDecomposeCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bridge theorems that connect fast compute
    phase success (`fast_compute_program_succeeds`) to the factored ParamHash
    obligation predicates introduced in
    `F4FPPVerifyParamHashBridgeDecomposeGoalsTheory`.

  Rationale
  - The factored ParamHash obligations (`paramhash_rep_ok`, `paramhash_stores_U_fast`,
    and their bundled form `paramhash_obligations_factored`) are useful as
    stable refinement targets.
  - Keeping the “execution ⇒ obligations” statements in this separate theory
    keeps `F4FPPVerifyParamHashBridgeDecomposeGoalsTheory` entirely OK.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeDecomposeCheatsGoals";

Theorem fast_compute_program_succeeds_imp_paramhash_rep_ok:
  !g. fast_compute_program_succeeds g ==> paramhash_rep_ok g
Proof
  (*
    Intended proof (later, without `cheat`):
    - relate the concrete SML `ParamHash.contains` and `ParamHash.list`
      observations for the post-state of `computeAllIntoParamHash`,
    - prove `contains p <=> MEM p (list())` for that post-state,
    - discharge any FFI coherence obligations needed for hashing/equality.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_stores_U_fast:
  !g. fast_compute_program_succeeds g ==> paramhash_stores_U_fast g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show the fast compute phase inserts *exactly* the parameters that
      constitute `U_fast g` (as defined in the goal layer),
    - this is an algorithmic/semantic argument, independent of hash-table
      representation correctness.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_factored:
  !g. fast_compute_program_succeeds g ==> paramhash_obligations_factored g
Proof
  rpt strip_tac
  \\ rw[paramhash_obligations_factored_def]
  >- (
    (* Wiring (`fast_param_set_is_paramhash`) is a future program-structure proof
       obligation; recorded as a placeholder here. *)
    cheat )
  >- metis_tac[fast_compute_program_succeeds_imp_paramhash_rep_ok]
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_stores_U_fast]
QED

val _ = export_theory ();

