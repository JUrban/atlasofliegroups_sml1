(*
  File: formal/cakeml/ParamHashInvariantGoalsScript.sml

  Purpose
  - Specify (and progressively prove) the key invariant-preservation facts for
    the pure-state ParamHash model in `ParamHashProgTheory`.
  - This is the next step needed to discharge the HOL4-side obligation
    `paramhash_state_ok` (see `formal/hol4/F4FPPVerifyParamHashBridgeStateRefineGoalsScript.sml`):
      we want to show that after building the table by repeated `match`/insert,
      the resulting state satisfies `ph_invariant`, and therefore its
      `contains` observation agrees with list membership.

  Status
  - The main “preserves invariant” theorem is currently recorded and `cheat`ed.
    The goal is to later replace it with a straightforward but somewhat
    tedious bucket/list reasoning proof.
*)

open HolKernel Parse boolLib bossLib;

open listTheory;
open pred_setTheory pred_setLib;

open ParamHashProgTheory;
open ParamHashSetGoalsTheory;

val _ = new_theory "ParamHashInvariantGoals";

(* ------------------------------------------------------------------------- *)
(*  Small list/nthn helper lemmas                                             *)
(* ------------------------------------------------------------------------- *)

Theorem nthn_append_lt:
  !xs ys n.
    n < LENGTH xs ==> nthn n (xs ++ ys) = nthn n xs
Proof
  Induct_on `xs`
  \\ rw[]
  \\ Cases_on `n`
  \\ simp[nthn_def]
  \\ first_x_assum match_mp_tac
  \\ simp[]
QED

Theorem nthn_append_sing_len:
  !xs x. nthn (LENGTH xs) (xs ++ [x]) = x
Proof
  Induct_on `xs`
  \\ simp[nthn_def]
QED

(* Build a state by inserting/matching every element of `ps` in order. *)
Definition ph_build_state_def:
  (ph_build_state ([]:num list) (s:ph_state) = s) /\
  (ph_build_state (p::ps) s = ph_build_state ps (SND (ph_match_state p s)))
End

(* Invariant preservation across a single `match_state` step. *)
Theorem ph_match_state_preserves_invariant:
  !p s. ph_invariant s ==> ph_invariant (SND (ph_match_state p s))
Proof
  (*
    Planned proof (later, without `cheat`):
    - split `ph_invariant` into `ph_ok`/`ph_bucketed`/`ph_covered`,
    - do a case split on `ph_lookup_state p s`,
    - in the NONE case, show:
        - `ph_ok` is preserved by the `LUPDATE`/append construction
          (using `nthn_append_lt` and `nthn_append_sing_len`),
        - `ph_bucketed` is preserved because only bucket `i` changes and the
          new head pair has `bucket_index p m = i`,
        - `ph_covered` is preserved for old indices (old pairs remain in `FLAT`)
          and extended for the new index by the inserted head pair.
  *)
  cheat
QED

(* Inductive corollary: building preserves the invariant. *)
Theorem ph_build_state_preserves_invariant:
  !ps s. ph_invariant s ==> ph_invariant (ph_build_state ps s)
Proof
  Induct_on `ps`
  \\ rw[ph_build_state_def]
  \\ metis_tac[ph_match_state_preserves_invariant]
QED

(* If we build from an invariant state, then the resulting “contains” predicate
   agrees with membership in the set of stored elements. *)
Theorem ph_contains_after_build_iff_set:
  !p ps s.
    ph_invariant s ==>
      (ph_contains_state p (ph_build_state ps s) <=> p IN ph_set (ph_build_state ps s))
Proof
  rpt strip_tac
  \\ match_mp_tac ph_contains_state_iff_set
  \\ metis_tac[ph_build_state_preserves_invariant]
QED

val _ = export_theory ();
