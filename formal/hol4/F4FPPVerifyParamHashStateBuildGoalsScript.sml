(*
  File: formal/hol4/F4FPPVerifyParamHashStateBuildGoalsScript.sml

  Purpose
  - Extend the HOL4-side ParamHash *state model* (`F4FPPVerifyParamHashStateGoalsTheory`)
    with a small pure “build by repeated insert/match” functional model.
  - Prove (no `cheat`) that this pure build process preserves the invariant
    `ph_invariant` under the basic Atlas hash-range contract.

  Why this matters
  - The bridge stack currently uses a cheated lemma of the form:
        `fast_compute_program_succeeds g ⇒ paramhash_state_ok g`
    where `paramhash_state_ok g` asserts there exists some abstract state `s`
    satisfying `ph_invariant` and matching the observable `(list,contains)` view
    of the concrete SML ParamHash.
  - For an eventual CakeML/translator proof, it is helpful to have an explicit
    *pure reference model* for what the imperative table is supposed to do:
        `ph_create_state m` followed by repeated `ph_match_state`.
    Then the remaining “hard” bridge lemma can be stated precisely as a
    refinement claim from imperative execution to this pure reference model.

  Scope / limitations
  - This file is about the HOL4-side `param`-typed model using `atlas_eq` and
    `atlas_hash_mod` (see `F4FPPVerifyParamHashStateGoalsTheory`), not the
    earlier CakeML-side numeric toy model.
  - We assume only `atlas_hash_range` to justify bucket-index bounds; equality
    coherence (`atlas_eq_is_hol_eq`, `atlas_hash_eq_ok`) is not needed for
    invariant preservation, only for representation correctness lemmas.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open rich_listTheory;
open optionTheory;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyParamHashStateGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashStateBuildGoals";

(* ------------------------------------------------------------------------- *)
(*  Small list helper                                                        *)
(* ------------------------------------------------------------------------- *)

Theorem EL_append_sing_len:
  !xs x. EL (LENGTH xs) (xs ++ [x]) = x
Proof
  Induct_on `xs`
  \\ simp[]
QED

(* Index-friendly membership in `FLAT`. *)
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
(*  Pure-state build model                                                   *)
(* ------------------------------------------------------------------------- *)

Definition ph_create_state_def:
  ph_create_state m =
    <| bucket_count := m; elems := ([]:param list); buckets := REPLICATE m ([]:(param # num) list) |>
End

Theorem ph_create_state_ok:
  !m. m <> 0 ==> ph_ok (ph_create_state m)
Proof
  rpt strip_tac
  \\ rw[ph_ok_def, ph_create_state_def]
  \\ simp[]
  \\ rpt strip_tac
  \\ fs[MEM_FLAT]
QED

Theorem ph_create_state_bucketed:
  !m. ph_bucketed (ph_create_state m)
Proof
  rw[ph_bucketed_def, ph_create_state_def]
  \\ `i < m` by simp[]
  \\ `EL i (REPLICATE m ([]:(param # num) list)) = []` by metis_tac[EL_REPLICATE]
  \\ fs[]
QED

Theorem ph_create_state_covered:
  !m. ph_covered (ph_create_state m)
Proof
  rw[ph_covered_def, ph_create_state_def]
QED

Theorem ph_create_state_invariant:
  !m. m <> 0 ==> ph_invariant (ph_create_state m)
Proof
  rpt strip_tac
  \\ rw[ph_invariant_def]
  \\ metis_tac[ph_create_state_ok, ph_create_state_bucketed, ph_create_state_covered]
QED

Definition ph_match_state_def:
  ph_match_state (p:param) (s:ph_state) =
    case ph_lookup_state p s of
      SOME idx => (idx,s)
    | NONE =>
        let i = bucket_index p s.bucket_count in
        let j = LENGTH s.elems in
        let b = EL i s.buckets in
        let bs' = LUPDATE ((p,j)::b) i s.buckets in
        let es' = s.elems ++ [p] in
          (j, s with <| elems := es'; buckets := bs' |>)
End

Definition ph_build_state_def:
  (ph_build_state ([]:param list) (s:ph_state) = s) /\
  (ph_build_state (p::ps) s = ph_build_state ps (SND (ph_match_state p s)))
End

(* ------------------------------------------------------------------------- *)
(*  Invariant preservation                                                    *)
(* ------------------------------------------------------------------------- *)

Theorem ph_match_state_preserves_ok:
  !p s. atlas_hash_range /\ ph_ok s ==> ph_ok (SND (ph_match_state p s))
Proof
  rpt strip_tac
  \\ Cases_on `ph_lookup_state p s`
  \\ simp[ph_match_state_def, LET_THM]
  \\ TRY (metis_tac[])
  \\ qabbrev_tac `i = bucket_index p s.bucket_count`
  \\ qabbrev_tac `j = LENGTH s.elems`
  \\ qabbrev_tac `b = EL i s.buckets`
  \\ qabbrev_tac `bs' = LUPDATE ((p,j)::b) i s.buckets`
  \\ qabbrev_tac `es' = s.elems ++ [p]`
  \\ `i < LENGTH s.buckets` by
       (fs[Abbr`i`] \\ metis_tac[bucket_index_lt_len_buckets])
  \\ simp[ph_ok_def]
  \\ rpt conj_tac
  >- fs[ph_ok_def]
  >- (simp[Abbr`bs'`, LENGTH_LUPDATE] \\ fs[ph_ok_def])
  \\ rpt gen_tac
  \\ strip_tac
  \\ fs[ph_ok_def]
  \\ rename1 `MEM (q,idx) (FLAT bs')`
  \\ qpat_x_assum `MEM (q,idx) (FLAT bs')`
       (mp_tac o MATCH_MP (iffLR MEM_FLAT_EL))
  \\ disch_then (qx_choose_then `k` strip_assume_tac)
  \\ `k < LENGTH s.buckets` by
       (qpat_x_assum `k < LENGTH bs'` mp_tac
        \\ simp[Abbr`bs'`, LENGTH_LUPDATE])
  \\ Cases_on `k = i`
  >- (
    fs[Abbr`bs'`, EL_LUPDATE]
    \\ Cases_on `(q,idx) = (p,j)`
    >- (
      fs[Abbr`j`, Abbr`es'`]
      \\ simp[LENGTH_APPEND, EL_append_sing_len]
      \\ decide_tac)
    \\ (
      `MEM (q,idx) b` by metis_tac[MEM]
      \\ `MEM (q,idx) (FLAT s.buckets)` by
           (simp[MEM_FLAT_EL]
            \\ qexists_tac `i`
            \\ conj_tac
            >- metis_tac[]
            \\ qpat_x_assum `MEM (q,idx) b` mp_tac
            \\ simp[Abbr`b`])
      \\ res_tac
      \\ fs[Abbr`es'`]
      \\ simp[LENGTH_APPEND, EL_APPEND1]
      \\ decide_tac))
  \\ (
    fs[Abbr`bs'`, EL_LUPDATE]
    \\ `MEM (q,idx) (FLAT s.buckets)` by
         (simp[MEM_FLAT_EL]
          \\ qexists_tac `k`
          \\ conj_tac
          >- metis_tac[]
          \\ qpat_x_assum `MEM (q,idx) (EL k s.buckets)` mp_tac
          \\ simp[])
    \\ res_tac
    \\ fs[Abbr`es'`]
    \\ simp[LENGTH_APPEND, EL_APPEND1]
    \\ decide_tac)
QED

Theorem ph_match_state_preserves_bucketed:
  !p s. atlas_hash_range /\ ph_invariant s ==> ph_bucketed (SND (ph_match_state p s))
Proof
  rpt strip_tac
  \\ Cases_on `ph_lookup_state p s`
  \\ simp[ph_match_state_def, LET_THM]
  \\ TRY (metis_tac[ph_invariant_def])
  \\ fs[ph_invariant_def]
  \\ simp[ph_bucketed_def]
  \\ qabbrev_tac `i = bucket_index p s.bucket_count`
  \\ qabbrev_tac `j = LENGTH s.elems`
  \\ qabbrev_tac `b = EL i s.buckets`
  \\ qabbrev_tac `bs' = LUPDATE ((p,j)::b) i s.buckets`
  \\ `i < LENGTH s.buckets` by
       (fs[Abbr`i`] \\ metis_tac[bucket_index_lt_len_buckets])
  \\ rw[]
  \\ rename1 `k < LENGTH _`
  \\ rename1 `MEM (q,idx) (EL k _)`
  \\ `k < LENGTH s.buckets` by fs[Abbr`bs'`, LENGTH_LUPDATE]
  \\ Cases_on `k = i` THENL
    [(
      fs[Abbr`bs'`, EL_LUPDATE]
      \\ Cases_on `(q,idx) = (p,j)` THENL
        [(qpat_x_assum `(q,idx) = (p,j)`
            (mp_tac o REWRITE_RULE [pairTheory.PAIR_EQ])
          \\ strip_tac
          \\ fs[Abbr`i`]),
         (
           `MEM (q,idx) b` by metis_tac[MEM]
           \\ `MEM (q,idx) (EL i s.buckets)` by fs[Abbr`b`]
           \\ fs[ph_bucketed_def]
           \\ metis_tac[]
         )]
     ),
     (
      fs[Abbr`bs'`, EL_LUPDATE]
      \\ fs[ph_bucketed_def]
      \\ res_tac
     )]
QED

Theorem ph_match_state_preserves_covered:
  !p s. atlas_hash_range /\ ph_invariant s ==> ph_covered (SND (ph_match_state p s))
Proof
  rpt strip_tac
  \\ Cases_on `ph_lookup_state p s`
  \\ simp[ph_match_state_def, LET_THM]
  \\ TRY (metis_tac[ph_invariant_def])
  \\ fs[ph_invariant_def]
  \\ simp[ph_match_state_def, LET_THM, ph_covered_def]
  \\ qabbrev_tac `i = bucket_index p s.bucket_count`
  \\ qabbrev_tac `j = LENGTH s.elems`
  \\ qabbrev_tac `b = EL i s.buckets`
  \\ qabbrev_tac `bs' = LUPDATE ((p,j)::b) i s.buckets`
  \\ qabbrev_tac `es' = s.elems ++ [p]`
  \\ `i < LENGTH s.buckets` by
       (fs[Abbr`i`] \\ metis_tac[bucket_index_lt_len_buckets])
  \\ rw[]
  \\ rename1 `t < j + 1`
  \\ Cases_on `t < j`
  >- (
    fs[Abbr`j`]
    \\ simp[Abbr`es'`, EL_APPEND1]
    \\ `MEM (EL t s.elems,t) (FLAT s.buckets)` by metis_tac[ph_covered_def]
    \\ `?k. k < LENGTH s.buckets /\ MEM (EL t s.elems,t) (EL k s.buckets)` by
         metis_tac[MEM_FLAT_EL]
    \\ pop_assum strip_assume_tac
    \\ simp[MEM_FLAT_EL]
    \\ qexists_tac `k`
    \\ conj_tac
    >- fs[Abbr`bs'`, LENGTH_LUPDATE]
    \\ Cases_on `k = i`
    >- (
      fs[Abbr`bs'`, EL_LUPDATE]
      \\ `MEM (EL t s.elems,t) b` by fs[Abbr`b`]
      \\ simp[MEM]
      \\ metis_tac[])
    \\ fs[Abbr`bs'`, EL_LUPDATE])
  \\ `t = j` by decide_tac
  \\ fs[Abbr`j`]
  \\ simp[Abbr`es'`, EL_append_sing_len]
  \\ simp[MEM_FLAT_EL]
  \\ qexists_tac `i`
  \\ conj_tac
  >- fs[Abbr`bs'`, LENGTH_LUPDATE]
  \\ fs[Abbr`bs'`, EL_LUPDATE]
  \\ simp[MEM]
QED

Theorem ph_match_state_preserves_invariant:
  !p s. atlas_hash_range /\ ph_invariant s ==> ph_invariant (SND (ph_match_state p s))
Proof
  rpt strip_tac
  \\ rw[ph_invariant_def]
  >- (
    match_mp_tac ph_match_state_preserves_ok
    \\ simp[]
    \\ fs[ph_invariant_def])
  >- metis_tac[ph_match_state_preserves_bucketed]
  \\ metis_tac[ph_match_state_preserves_covered]
QED

Theorem ph_build_state_preserves_invariant:
  !ps s. atlas_hash_range /\ ph_invariant s ==> ph_invariant (ph_build_state ps s)
Proof
  Induct_on `ps`
  \\ rw[ph_build_state_def]
  \\ metis_tac[ph_match_state_preserves_invariant]
QED

Definition ph_build_from_create_state_def:
  ph_build_from_create_state m ps = ph_build_state ps (ph_create_state m)
End

Theorem ph_build_from_create_state_invariant:
  !m ps. atlas_hash_range /\ m <> 0 ==> ph_invariant (ph_build_from_create_state m ps)
Proof
  rpt strip_tac
  \\ simp[ph_build_from_create_state_def]
  \\ match_mp_tac ph_build_state_preserves_invariant
  \\ simp[ph_create_state_invariant]
QED

val _ = export_theory ();
