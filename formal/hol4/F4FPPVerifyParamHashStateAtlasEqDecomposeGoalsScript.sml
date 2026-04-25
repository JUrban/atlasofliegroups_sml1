(*
  File: formal/hol4/F4FPPVerifyParamHashStateAtlasEqDecomposeGoalsScript.sml

  Purpose
  - Make the modulo-`atlas_eq` ParamHash state proof more explicit by
    decomposing the (currently `cheat`ed) direction:

      `mem_atlas_eq p s.elems ==> ph_contains_state p s`

    into a small set of named intermediate lemmas that correspond closely to
    the intended informal proof steps.

  Why this is useful
  - `F4FPPVerifyParamHashStateGoalsTheory` already contains the end-to-end
    statement `ph_contains_state_iff_mem_atlas_eq_elems`, but the hard direction
    is currently recorded as a single `cheat` with an informal proof sketch.
  - For top-down verification planning (and for eventual CakeML translator
    proofs), it helps to expose the *exact* “hinge points”:
      - how `ph_covered` and `ph_ok` connect `MEM q elems` to a bucket entry,
      - how `ph_bucketed` identifies the correct bucket index for `q`,
      - how `atlas_hash_respects_eq` transfers that index from `q` to `p`,
      - and a small pure lemma about `find_in_bucket` completeness modulo
        `atlas_eq`.

  Status
  - Many sublemmas here are proved (“OK”).
  - The key pure lemma about `find_in_bucket` completeness modulo `atlas_eq` is
    recorded but left as `cheat` for now; once discharged, the main lemma in
    this file should become entirely “OK”.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;
open optionTheory;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqListGoalsTheory;
open F4FPPVerifyParamHashStateGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashStateAtlasEqDecomposeGoals";

(* ------------------------------------------------------------------------- *)
(*  A small pure predicate about buckets                                      *)
(* ------------------------------------------------------------------------- *)

Definition bucket_has_atlas_eq_def:
  bucket_has_atlas_eq (p:param) (b:(param # num) list) <=>
    ?q idx. MEM (q,idx) b /\ atlas_eq p q
End

(* Key pure lemma: if a bucket contains an element semantically equal to `p`,
   then `find_in_bucket p b` succeeds.

   This is a pure list lemma about the definition of `find_in_bucket` and does
   not depend on ParamHash invariants. *)
Theorem bucket_has_atlas_eq_imp_find_in_bucket_SOME:
  !p b. bucket_has_atlas_eq p b ==> ?idx. find_in_bucket p b = SOME idx
Proof
  rpt gen_tac
  \\ strip_tac
  \\ Cases_on `find_in_bucket p b`
  >- (
    qpat_x_assum `bucket_has_atlas_eq p b`
         (qx_choose_then `q` (qx_choose_then `j` strip_assume_tac) o
          REWRITE_RULE[bucket_has_atlas_eq_def])
    \\ `~atlas_eq p q` by metis_tac[find_in_bucket_NONE_imp_all_not_eq]
    \\ fs[]
    )
  \\ qexists_tac `x`
  \\ simp[]
QED

(* ------------------------------------------------------------------------- *)
(*  Invariant plumbing lemmas (OK)                                            *)
(* ------------------------------------------------------------------------- *)

Theorem mem_elems_imp_exists_index:
  !q (s:ph_state). MEM q s.elems ==> ?j. j < LENGTH s.elems /\ EL j s.elems = q
Proof
  rpt strip_tac
  \\ qpat_x_assum `MEM q s.elems` (mp_tac o MATCH_MP (iffLR MEM_EL))
  \\ disch_then (qx_choose_then `j` strip_assume_tac)
  \\ qexists_tac `j`
  \\ simp[]
QED

Theorem ph_covered_and_index_imp_MEM_flat:
  !s j.
    ph_covered s /\ j < LENGTH s.elems ==>
      MEM (EL j s.elems, j) (FLAT s.buckets)
Proof
  rw[ph_covered_def]
QED

Theorem MEM_flat_imp_exists_bucket_index:
  !x bs.
    MEM x (FLAT bs) ==> ?b. MEM b bs /\ MEM x b
Proof
  rw[MEM_FLAT] \\ metis_tac[]
QED

Theorem MEM_bucket_imp_exists_EL_index:
  !b bs. MEM b bs ==> ?i. i < LENGTH bs /\ EL i bs = b
Proof
  rpt strip_tac
  \\ qpat_x_assum `MEM b bs` (mp_tac o MATCH_MP (iffLR MEM_EL))
  \\ disch_then (qx_choose_then `i` strip_assume_tac)
  \\ qexists_tac `i`
  \\ simp[]
QED

Theorem ph_bucketed_MEM_bucket_imp_bucket_index:
  !s i p idx.
    ph_bucketed s /\ i < LENGTH s.buckets /\ MEM (p,idx) (EL i s.buckets) ==>
      bucket_index p s.bucket_count = i
Proof
  rpt strip_tac
  \\ fs[ph_bucketed_def]
  \\ metis_tac[]
QED

(* ------------------------------------------------------------------------- *)
(*  Main decomposed lemma (CHEAT-tainted only via the pure bucket lemma)       *)
(* ------------------------------------------------------------------------- *)

Theorem mem_atlas_eq_elems_imp_ph_contains_state_decomposed:
  !p s.
    atlas_hash_eq_ok /\ ph_invariant s /\ mem_atlas_eq p s.elems ==>
      ph_contains_state p s
Proof
  rpt gen_tac
  \\ strip_tac
  \\ qpat_x_assum `mem_atlas_eq p s.elems`
       (qx_choose_then `q` strip_assume_tac o REWRITE_RULE[mem_atlas_eq_def])
  \\ fs[ph_invariant_def]
  \\ drule mem_elems_imp_exists_index
  \\ disch_then (qx_choose_then `j` strip_assume_tac)
  \\ `MEM (EL j s.elems, j) (FLAT s.buckets)` by
       metis_tac[ph_covered_and_index_imp_MEM_flat]
  \\ `MEM (q,j) (FLAT s.buckets)` by metis_tac[]
  \\ qpat_x_assum `MEM (q,j) (FLAT s.buckets)`
       (mp_tac o MATCH_MP MEM_flat_imp_exists_bucket_index)
  \\ disch_then (qx_choose_then `b` strip_assume_tac)
  \\ qpat_x_assum `MEM b s.buckets`
       (mp_tac o MATCH_MP MEM_bucket_imp_exists_EL_index)
  \\ disch_then (qx_choose_then `i` strip_assume_tac)
  \\ `MEM (q,j) (EL i s.buckets)` by simp[]
  \\ `bucket_index q s.bucket_count = i` by
       metis_tac[ph_bucketed_MEM_bucket_imp_bucket_index]
  \\ `bucket_index p s.bucket_count = i` by
       (fs[atlas_hash_eq_ok_def, atlas_hash_respects_eq_def, bucket_index_def]
        \\ metis_tac[])
  \\ `bucket_has_atlas_eq p (EL i s.buckets)` by
       (rw[bucket_has_atlas_eq_def] \\ metis_tac[])
  \\ drule bucket_has_atlas_eq_imp_find_in_bucket_SOME
  \\ disch_then (qx_choose_then `idx` strip_assume_tac)
  \\ `s.bucket_count <> 0` by fs[ph_ok_def]
  \\ `find_in_bucket p b = SOME idx` by metis_tac[]
  \\ rw[ph_contains_state_def]
  \\ qexists_tac `idx`
  \\ simp[ph_lookup_state_def, LET_THM]
  \\ simp[]
QED

val _ = export_theory ();
