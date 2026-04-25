(*
  File: formal/cakeml/ParamHashBuildGoalsScript.sml

  Purpose
  - Provide a small, *non-cheated* “pure build” layer shared by:
      - `ParamHashInvariantGoalsTheory` (invariant preservation goals), and
      - `ParamHashRefinementGoalsTheory` (monadic ⇔ pure-state refinement).

  Why this file exists
  - This isolates small “pure build” facts (list/nthn helpers, `ph_build_state`,
    and `ph_match_state_preserves_ok`) so that refinement/end-to-end layers can
    re-use them without pulling in heavier proof dependencies.

  Contents
  - Two list/nthn helper lemmas used when reasoning about appending a fresh
    element to `elems` while preserving properties about existing indices.
  - A key well-formedness preservation lemma:
      `ph_match_state_preserves_ok` (pure `match` step preserves `ph_ok`).
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

(* A more index-friendly characterisation of membership in `FLAT`.

   This is frequently useful when reasoning about `LUPDATE` on a list-of-lists:
   turning `MEM x (FLAT xss)` into an explicit bucket index lets us case split
   on whether the updated position is the witness position. *)
Theorem MEM_FLAT_EL:
  !x xss. MEM x (FLAT xss) <=> ?i. i < LENGTH xss /\ MEM x (EL i xss)
Proof
  rw[MEM_FLAT]
  \\ eq_tac
  >- (
    strip_tac
    \\ rename1 `MEM ys xss`
    \\ `?i. i < LENGTH xss /\ (EL i xss = ys)` by metis_tac[MEM_EL]
    \\ pop_assum strip_assume_tac
    \\ qexists_tac `i`
    \\ fs[] )
  \\ strip_tac
  \\ qexists_tac `EL i xss`
  \\ simp[EL_MEM]
QED

(* ------------------------------------------------------------------------- *)
(*  Well-formedness preservation                                              *)
(* ------------------------------------------------------------------------- *)

(* A single pure `match` step preserves the basic well-formedness invariant. *)
Theorem ph_match_state_preserves_ok:
  !p s. ph_ok s ==> ph_ok (SND (ph_match_state p s))
Proof
  rpt strip_tac
  \\ Cases_on `ph_lookup_state p s`
  >- (
    simp[ph_match_state_def, LET_THM]
    \\ qabbrev_tac `m = s.bucket_count`
    \\ qabbrev_tac `i = bucket_index p m`
    \\ qabbrev_tac `j = s.count`
    \\ qabbrev_tac `b = EL i s.buckets`
    \\ `i < LENGTH s.buckets` by
         (fs[ph_ok_def, Abbr`i`, Abbr`m`] \\ metis_tac[bucket_index_lt])
    \\ simp[ph_ok_def]
    \\ rpt conj_tac
    >- fs[ph_ok_def, Abbr`m`]
    >- (fs[ph_ok_def, Abbr`m`] \\ simp[LENGTH_LUPDATE])
    >- (fs[ph_ok_def, Abbr`j`] \\ simp[LENGTH_APPEND] \\ decide_tac)
    \\ rpt strip_tac
    \\ rename1 `MEM (q,idx) (FLAT _)`
    \\ fs[MEM_FLAT]
    \\ rename1 `MEM l (LUPDATE ((p,j)::b) i s.buckets)`
    \\ rename1 `MEM (q,idx) l`
    \\ `l = ((p,j)::b) \/ MEM l s.buckets` by
         metis_tac[MEM_LUPDATE_E]
    \\ (Cases_on `l = ((p,j)::b)` THENL
        [fs[]
         \\ fs[MEM]
         >- (
           fs[ph_ok_def, Abbr`j`]
           \\ simp[LENGTH_APPEND, nthn_append_sing_len]
           \\ decide_tac)
         \\ `MEM (q,idx) b` by metis_tac[MEM]
         \\ `MEM (q,idx) (FLAT s.buckets)` by
              (simp[MEM_FLAT]
               \\ qexists_tac `EL i s.buckets`
               \\ (conj_tac THENL
                    [metis_tac[EL_MEM], fs[Abbr`b`]]))
         \\ `idx < LENGTH s.elems /\ nthn idx s.elems = q` by
              metis_tac[ph_ok_def]
         \\ simp[LENGTH_APPEND, nthn_append_lt]
         \\ decide_tac,
         fs[]
         \\ `MEM l s.buckets` by metis_tac[MEM_LUPDATE_E]
         \\ `MEM (q,idx) (FLAT s.buckets)` by
              (simp[MEM_FLAT]
               \\ qexists_tac `l`
               \\ fs[])
         \\ `idx < LENGTH s.elems /\ nthn idx s.elems = q` by
              metis_tac[ph_ok_def]
         \\ simp[LENGTH_APPEND, nthn_append_lt]
         \\ decide_tac])
    )
  \\ fs[ph_match_state_def]
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
