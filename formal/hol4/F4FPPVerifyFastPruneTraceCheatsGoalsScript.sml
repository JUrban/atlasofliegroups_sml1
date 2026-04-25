(*
  File: formal/hol4/F4FPPVerifyFastPruneTraceCheatsGoalsScript.sml

  Purpose
  - Record “translator target” bridge lemmas for the fast domain pruning story,
    phrased in terms of the list witness `fast_domain_trace g` introduced in
    `F4FPPVerifyFastPruneTraceGoalsTheory`.

  Motivation
  - Proving `fast_domain_is_pruned g` directly is a set-equality goal about
    `D_fast g`.
  - In practice, a CakeML/translator proof will likely extract or compute a
    concrete list of triples the fast code iterated, and then establish:
      - this list’s set view is `D_fast g`, and
      - it matches the intended pruned domain (sound + complete).

  Status
  - The bridge lemmas here are currently `cheat`ed placeholders.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyFastPruneTraceGoalsTheory;

val _ = new_theory "F4FPPVerifyFastPruneTraceCheatsGoals";

Theorem fast_compute_program_succeeds_imp_fast_domain_trace_ok:
  !g.
    fast_compute_program_succeeds g ==>
      fast_domain_trace_ok g
Proof
  (*
    Intended proof (later, without `cheat`):
    - connect the concrete compute-phase triple enumeration loop(s) to the
      abstract domain set `D_fast g` by showing that the extracted list
      `fast_domain_trace g` enumerates exactly the considered triples.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_fast_domain_trace_sound:
  !g.
    fast_compute_program_succeeds g ==>
      fast_domain_trace_sound g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show every triple actually iterated by the compute phase satisfies the
      pruning predicate, i.e. belongs to `D_fast_pruned g`.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_fast_domain_trace_complete:
  !g.
    fast_compute_program_succeeds g ==>
      fast_domain_trace_complete g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show the enumeration is complete for the pruning predicate: every triple
      in `D_fast_pruned g` appears (set-wise) in the trace list.
  *)
  cheat
QED

val _ = export_theory ();

