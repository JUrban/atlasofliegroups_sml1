(*
  File: formal/hol4/F4FPPVerifyFastPruneTraceGoalsScript.sml

  Purpose
  - Add a “trace/list witness” layer for the fast program’s enumeration domain.

  Motivation
  - The existing fast pruning obligation `fast_domain_is_pruned g` is stated as
      `D_fast g = D_fast_pruned g`,
    i.e. an equality between sets of triples.
  - For translator/CakeML proofs it is often easier to talk about a concrete
    list produced by execution, and then reason about its set view.

  What this theory introduces
  - An abstract list-valued witness `fast_domain_trace g : triple list` intended
    to model “the triples actually iterated/considered by the fast compute
    phase”.
  - A set-view `D_fast_trace g = set (fast_domain_trace g)`.
  - Small obligations stating:
      (1) this trace’s set view is exactly `D_fast g`, and
      (2) this trace’s set view is included in / covers the pruned domain.

  Status
  - OK (no `cheat`): definitions + logical consequences only.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyFastRefineGoalsTheory;
open F4FPPVerifyFastPruneGoalsTheory;
open F4FPPVerifyFastPruneDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifyFastPruneTraceGoals";

(* Abstract list witness of the fast compute phase’s triple enumeration. *)
val _ = new_constant ("fast_domain_trace", ``:group -> triple list``);

Definition D_fast_trace_def:
  D_fast_trace g : triple set = set (fast_domain_trace g)
End

(* The trace list represents the abstract fast domain set `D_fast`. *)
Definition fast_domain_trace_ok_def:
  fast_domain_trace_ok g <=>
    D_fast g = D_fast_trace g
End

(* Soundness/completeness of the trace w.r.t. the pruned domain. *)
Definition fast_domain_trace_sound_def:
  fast_domain_trace_sound g <=>
    D_fast_trace g SUBSET D_fast_pruned g
End

Definition fast_domain_trace_complete_def:
  fast_domain_trace_complete g <=>
    D_fast_pruned g SUBSET D_fast_trace g
End

Theorem fast_domain_trace_sound_and_complete_imp_trace_eq_pruned:
  !g.
    fast_domain_trace_sound g /\ fast_domain_trace_complete g ==>
      D_fast_trace g = D_fast_pruned g
Proof
  rw[fast_domain_trace_sound_def, fast_domain_trace_complete_def]
  \\ metis_tac[SUBSET_ANTISYM_EQ]
QED

Theorem fast_domain_trace_ok_and_trace_sound_imp_fast_domain_sound:
  !g.
    fast_domain_trace_ok g /\ fast_domain_trace_sound g ==>
      fast_domain_sound g
Proof
  rw[fast_domain_trace_ok_def, fast_domain_trace_sound_def, fast_domain_sound_def]
QED

Theorem fast_domain_trace_ok_and_trace_complete_imp_fast_domain_complete:
  !g.
    fast_domain_trace_ok g /\ fast_domain_trace_complete g ==>
      fast_domain_complete g
Proof
  rw[fast_domain_trace_ok_def, fast_domain_trace_complete_def, fast_domain_complete_def]
QED

Theorem fast_domain_trace_ok_and_trace_sound_and_complete_imp_fast_domain_is_pruned:
  !g.
    fast_domain_trace_ok g /\
    fast_domain_trace_sound g /\
    fast_domain_trace_complete g ==>
      fast_domain_is_pruned g
Proof
  rpt strip_tac
  \\ match_mp_tac fast_domain_sound_and_complete_imp_fast_domain_is_pruned
  \\ conj_tac
  >- metis_tac[fast_domain_trace_ok_and_trace_sound_imp_fast_domain_sound]
  \\ metis_tac[fast_domain_trace_ok_and_trace_complete_imp_fast_domain_complete]
QED

val _ = export_theory ();

