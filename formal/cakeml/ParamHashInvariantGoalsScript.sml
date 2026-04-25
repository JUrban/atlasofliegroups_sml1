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
    The goal is to replace it with a straightforward bucket/list reasoning
    proof (see the comment inside the theorem).

  Note
  - Basic “pure build” definitions/lemmas live in `ParamHashBuildGoalsTheory`,
    so that non-cheated theories can re-use them without importing this (currently
    cheat-tainted) file.
*)

open HolKernel Parse boolLib bossLib;

open listTheory;
	open pred_setTheory pred_setLib;

	open ParamHashProgTheory;
	open ParamHashBuildGoalsTheory;
	open ParamHashSetGoalsTheory;

val _ = new_theory "ParamHashInvariantGoals";

(* ------------------------------------------------------------------------- *)
(*  Invariant preservation goals                                              *)
(* ------------------------------------------------------------------------- *)

(* Invariant preservation across a single `match_state` step. *)
Theorem ph_match_state_preserves_invariant:
  !p s. ph_invariant s ==> ph_invariant (SND (ph_match_state p s))
Proof
  (*
    Planned proof (without `cheat`):
    - split `ph_invariant` into `ph_ok`/`ph_bucketed`/`ph_covered`,
    - do a case split on `ph_lookup_state p s`,
    - in the NONE case, show:
        - `ph_ok` is preserved (use `ph_match_state_preserves_ok`),
        - `ph_bucketed` is preserved by `EL_LUPDATE` + `ph_bucketed` on the tail,
        - `ph_covered` is preserved using `MEM_FLAT` witnesses and `EL_LUPDATE`
          (old indices remain; the new last index is covered by the inserted head).
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
