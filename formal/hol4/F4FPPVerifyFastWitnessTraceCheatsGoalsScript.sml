(*
  File: formal/hol4/F4FPPVerifyFastWitnessTraceCheatsGoalsScript.sml

  Purpose
  - Record “translator target” bridge lemmas for the fast witness obligations,
    using the witness/insert trace layer introduced in
    `F4FPPVerifyFastWitnessTraceGoalsTheory`.

  Status
  - The bridge lemmas are currently `cheat`ed placeholders.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyFastWitnessTraceGoalsTheory;

val _ = new_theory "F4FPPVerifyFastWitnessTraceCheatsGoals";

Theorem fast_compute_program_succeeds_imp_fast_insert_trace_sound:
  !g.
    fast_compute_program_succeeds g ==>
      fast_insert_trace_sound g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show each event recorded in `fast_insert_trace g` corresponds to a real
      execution step producing `pi` as `first_final_term (mk_param g t)` for
      some considered triple `t` (and that `t` satisfies `fast_considers`).
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_fast_insert_trace_covers_U_fast:
  !g.
    fast_compute_program_succeeds g ==>
      fast_insert_trace_covers_U_fast g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show every element of the abstract fast set `U_fast g` arises from some
      insertion/match step and therefore appears in the event trace.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_fast_insert_trace_unitary:
  !g.
    fast_compute_program_succeeds g ==>
      fast_insert_trace_unitary g
Proof
  (*
    Intended proof (later, without `cheat`):
    - connect the SML unitarity filter (or bottom-layer predicate used when
      deciding to insert) to the abstract `is_unitary`.
  *)
  cheat
QED

val _ = export_theory ();

