(*
  File: formal/hol4/F4FPPVerifyFastWitnessTraceGoalsScript.sml

  Purpose
  - Add a “witness trace” layer for the fast compute phase: a list of concrete
    witness triples paired with the parameter they justify.

  Motivation
  - The pruning/witness obligation `fast_witnessed_pruned_exists g` is already a
    good semantic statement, but it quantifies over an abstract existence
    `?t. ...` for each stored `pi ∈ U_fast g`.
  - In the concrete implementation, each stored element typically arises from a
    concrete computation step: some triple `(x,lambda,gamma)` is considered and
    yields a `param` (or its first final term), which is then inserted.
  - Translator/CakeML proofs often prefer to reason about such a concrete trace
    of events and then derive the existential statements.

  What this theory introduces
  - An abstract list `fast_insert_trace g : (triple # param) list` intended to
    model “the compute phase produced `pi` from witness triple `t` (and attempted
    insertion/match)”.
  - Small obligations about this trace:
      - a per-event soundness predicate (each event is correctly justified), and
      - a coverage predicate (every `pi ∈ U_fast g` appears in the trace).
  - OK lemmas deriving `fast_witnessed_pruned_exists` and `fast_unitary_set`.

  Status
  - OK (no `cheat`): definitions + logical consequences only.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPVerifyFastPruneGoalsTheory;
open F4FPPVerifyFastPruneDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifyFastWitnessTraceGoals";

(* Abstract witness/insert trace of the compute phase. *)
val _ = new_constant ("fast_insert_trace", ``:group -> (triple # param) list``);

Definition fast_insert_trace_event_ok_def:
  fast_insert_trace_event_ok g (tp:triple # param) <=>
    (FST tp) IN D_slow g /\
    fast_considers g (FST tp) /\
    first_final_term (mk_param g (FST tp)) = SOME (SND tp)
End

Definition fast_insert_trace_sound_def:
  fast_insert_trace_sound g <=>
    !tp. MEM tp (fast_insert_trace g) ==> fast_insert_trace_event_ok g tp
End

(* Coverage: every stored element appears as the param component of some event. *)
Definition fast_insert_trace_covers_U_fast_def:
  fast_insert_trace_covers_U_fast g <=>
    !pi. pi IN U_fast g ==> ?t. MEM (t,pi) (fast_insert_trace g)
End

(* Optional extra check: all witnessed params are unitary. *)
Definition fast_insert_trace_unitary_def:
  fast_insert_trace_unitary g <=>
    !t pi. MEM (t,pi) (fast_insert_trace g) ==> is_unitary pi
End

Theorem fast_insert_trace_sound_and_covers_imp_fast_witnessed_pruned_exists:
  !g.
    fast_insert_trace_sound g /\
    fast_insert_trace_covers_U_fast g ==>
      fast_witnessed_pruned_exists g
Proof
  rw[fast_witnessed_pruned_exists_def]
  \\ fs[fast_insert_trace_covers_U_fast_def]
  \\ first_x_assum drule
  \\ disch_then (qx_choose_then `t` assume_tac)
  \\ qexists_tac `t`
  \\ fs[fast_insert_trace_sound_def]
  \\ first_x_assum drule
  \\ fs[fast_insert_trace_event_ok_def]
QED

Theorem fast_insert_trace_unitary_and_covers_imp_fast_unitary_set:
  !g.
    fast_insert_trace_unitary g /\
    fast_insert_trace_covers_U_fast g ==>
      fast_unitary_set g
Proof
  rw[fast_unitary_set_def,
     fast_insert_trace_unitary_def,
     fast_insert_trace_covers_U_fast_def]
  \\ first_x_assum drule
  \\ strip_tac
  \\ metis_tac[]
QED

val _ = export_theory ();
