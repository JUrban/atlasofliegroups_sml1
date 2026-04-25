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
  - The main “preserves invariant” theorem is proved (no `cheat`), and this
    theory builds as **OK**.

  Note
  - Basic “pure build” definitions/lemmas live in `ParamHashBuildGoalsTheory`,
    so that later theories can re-use them without depending on the entire
    invariant layer.
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
    Proof outline:
    - split `ph_invariant` into `ph_ok`/`ph_bucketed`/`ph_covered`,
    - do a case split on `ph_lookup_state p s`,
    - in the NONE case, show:
        - `ph_ok` is preserved (use `ph_match_state_preserves_ok`),
        - `ph_bucketed` is preserved by `EL_LUPDATE` + `ph_bucketed` on the tail,
        - `ph_covered` is preserved using `MEM_FLAT` witnesses and `EL_LUPDATE`
          (old indices remain; the new last index is covered by the inserted head).
  *)
  rpt strip_tac
  \\ Cases_on `ph_lookup_state p s`
  >- (
    fs[ph_invariant_def]
    \\ simp[ph_invariant_def]
    \\ rpt conj_tac
    >- metis_tac[ph_match_state_preserves_ok]
    >- (
      simp[ph_match_state_def, LET_THM]
      \\ qabbrev_tac `i = bucket_index p s.bucket_count`
      \\ qabbrev_tac `j = s.count`
      \\ qabbrev_tac `b = EL i s.buckets`
      \\ qabbrev_tac `bs' = LUPDATE ((p,j)::b) i s.buckets`
      \\ `i < LENGTH s.buckets` by
           (fs[ph_ok_def, Abbr`i`] \\ metis_tac[bucket_index_lt])
      \\ qpat_x_assum `ph_bucketed s`
           (fn th => assume_tac (REWRITE_RULE[ph_bucketed_def] th))
      \\ rw[ph_bucketed_def]
      \\ rename1 `k < LENGTH _`
      \\ rename1 `MEM (q,idx) (EL k _)`
      \\ `k < LENGTH s.buckets` by fs[Abbr`bs'`, LENGTH_LUPDATE]
      \\ Cases_on `k = i`
      >- (
        fs[]
        \\ fs[Abbr`bs'`, EL_LUPDATE]
        \\ Cases_on `(q,idx) = (p,j)`
        >- fs[Abbr`i`]
        \\ `MEM (q,idx) b` by fs[MEM]
        \\ qpat_x_assum
             `!i p idx.
                i < LENGTH s.buckets /\ MEM (p,idx) (EL i s.buckets) ==>
                  bucket_index p s.bucket_count = i`
             (match_mp_tac o SPECL [``i:num``, ``q:num``, ``idx:num``])
        \\ simp[Abbr`b`])
      \\ fs[Abbr`bs'`, EL_LUPDATE]
      \\ qpat_x_assum
           `!i p idx.
              i < LENGTH s.buckets /\ MEM (p,idx) (EL i s.buckets) ==>
                bucket_index p s.bucket_count = i`
           (match_mp_tac o SPECL [``k:num``, ``q:num``, ``idx:num``])
      \\ simp[])
    >- (
      simp[ph_match_state_def, LET_THM]
      \\ qabbrev_tac `i = bucket_index p s.bucket_count`
      \\ qabbrev_tac `j = s.count`
      \\ qabbrev_tac `b = EL i s.buckets`
      \\ qabbrev_tac `bs' = LUPDATE ((p,j)::b) i s.buckets`
      \\ qabbrev_tac `ps' = s.elems ++ [p]`
      \\ `i < LENGTH s.buckets` by
           (fs[ph_ok_def, Abbr`i`] \\ metis_tac[bucket_index_lt])
      \\ rw[ph_covered_def]
      \\ rename1 `t < LENGTH _`
      \\ Cases_on `t < LENGTH s.elems`
      >- (
        simp[Abbr`ps'`, nthn_append_lt]
        \\ `MEM (nthn t s.elems,t) (FLAT s.buckets)` by metis_tac[ph_covered_def]
        \\ `?k. k < LENGTH s.buckets /\ MEM (nthn t s.elems,t) (EL k s.buckets)` by
             metis_tac[MEM_FLAT_EL]
        \\ pop_assum strip_assume_tac
        \\ simp[MEM_FLAT_EL]
        \\ qexists_tac `k`
        \\ conj_tac
        >- fs[Abbr`bs'`, LENGTH_LUPDATE]
        \\ Cases_on `k = i`
        >- (
          fs[Abbr`bs'`, EL_LUPDATE]
          \\ simp[MEM]
          \\ disj2_tac
          \\ fs[Abbr`b`])
        \\ fs[Abbr`bs'`, EL_LUPDATE])
      \\ `t = LENGTH s.elems` by
           (fs[Abbr`ps'`, LENGTH_APPEND] \\ decide_tac)
      \\ fs[ph_ok_def, Abbr`j`]
      \\ simp[Abbr`ps'`, nthn_append_sing_len]
      \\ simp[MEM_FLAT_EL]
      \\ qexists_tac `i`
      \\ conj_tac
      >- fs[Abbr`bs'`, LENGTH_LUPDATE]
      \\ fs[Abbr`bs'`, EL_LUPDATE]
      \\ simp[MEM])
    )
  \\ fs[ph_match_state_def]
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
