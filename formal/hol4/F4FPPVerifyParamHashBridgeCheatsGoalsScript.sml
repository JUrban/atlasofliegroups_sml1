(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bridge theorem connecting fast compute
    phase success to the bundled ParamHash obligation predicate `paramhash_ok`.

  Rationale
  - Keeping these “execution ⇒ obligations” statements out of
    `F4FPPVerifyParamHashBridgeGoalsTheory` allows that core theory to remain
    entirely OK (definitions + logical implications only), which in turn helps
    avoid unnecessary CHEAT-taint propagation.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeCheatsGoals";

Theorem fast_compute_program_succeeds_imp_paramhash_ok:
  !g. fast_compute_program_succeeds g ==> paramhash_ok g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - relate the concrete `ParamHash.t` post-state after `computeAllIntoParamHash`
      to `paramhash_list` and `paramhash_contains`,
    - prove `ParamHash.list` enumerates exactly the stored elements,
    - prove `ParamHash.contains` matches membership in that stored set,
    - connect that stored-set view to `U_fast g` (goal-layer fast set).
  *)
  cheat
QED

val _ = export_theory ();

