(*
  File: formal/cakeml/ParamHashBuildGoalsScript.sml

  Purpose
  - Provide a small, *non-cheated* “pure build” layer shared by:
      - `ParamHashInvariantGoalsTheory` (invariant preservation goals), and
      - `ParamHashRefinementGoalsTheory` (monadic ⇔ pure-state refinement).

  Why this file exists
  - `ParamHashInvariantGoalsTheory` intentionally contains `cheat`ed theorems.
    If we want a clean refinement layer, we must avoid importing a cheated
    theory just to access basic helper lemmas/definitions.

  Contents
  - Two list/nthn helper lemmas used when reasoning about appending a fresh
    element to `elems` while preserving properties about existing indices.
  - A pure-state build function `ph_build_state` that iterates `ph_match_state`
    over a list of parameters.

  Status
  - Intended to remain **OK** (no `cheat`): a stable base for later proofs.
*)

open HolKernel Parse boolLib bossLib;

open listTheory;

open ParamHashProgTheory;

val _ = new_theory "ParamHashBuildGoals";

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

(* ------------------------------------------------------------------------- *)
(*  Pure-state build                                                         *)
(* ------------------------------------------------------------------------- *)

(* Build a state by inserting/matching every element of `ps` in order. *)
Definition ph_build_state_def:
  (ph_build_state ([]:num list) (s:ph_state) = s) /\
  (ph_build_state (p::ps) s = ph_build_state ps (SND (ph_match_state p s)))
End

val _ = export_theory ();

