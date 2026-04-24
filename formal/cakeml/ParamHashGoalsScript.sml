(*
  File: formal/cakeml/ParamHashGoalsScript.sml

  Purpose
  - This file is intentionally “top-down”: it collects the *eventual*
    correctness statements we want about the ParamHash model from
    `ParamHashProgTheory`, without forcing us to discharge the proofs yet.
  - It is meant to support the staged plan in `VERIFY_ESTIMATE.md`:
      Stage A: identify the key lemmas needed to relate the fast (hash-based)
               and slow (list-based) checkers.
      Stage B: prove these lemmas, and then connect to the CakeML translator’s
               stateful semantics.

  Status
  - Most theorems in this theory are currently `cheat`ed on purpose.
    The value is in nailing down statements and interfaces early.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;
open listTheory;

open ParamHashProgTheory;

val _ = new_theory "ParamHashGoals";

(* ------------------------------------------------------------------------- *)
(*  Pure-state completeness goals                                             *)
(* ------------------------------------------------------------------------- *)

(* Completeness of lookup w.r.t. `elems` membership under the intended invariant.

   This is the key lemma needed to show that the hash-table membership test is
   complete (no false negatives) once the state has been built correctly. *)
Theorem ph_lookup_state_complete:
  !p s. ph_invariant s /\ MEM (p:num) s.elems ==> ?idx. ph_lookup_state p s = SOME idx
Proof
  rpt strip_tac
  \\ fs[ph_invariant_def]
  \\ `?n. n < LENGTH s.elems /\ (nthn n s.elems = p)` by
       metis_tac[MEM_imp_exists_nth]
  \\ `MEM (p,n) (FLAT s.buckets)` by
       (fs[ph_covered_def] \\ metis_tac[])
  \\ fs[MEM_FLAT]
  \\ rename1 `MEM (p,n) b`
  \\ rename1 `MEM b s.buckets`
  \\ `?i. i < LENGTH s.buckets /\ (EL i s.buckets = b)` by
       metis_tac[MEM_EL]
  \\ `bucket_index p s.bucket_count = i` by
       (fs[ph_bucketed_def] \\ metis_tac[])
  \\ `EL (bucket_index p s.bucket_count) s.buckets = b` by metis_tac[]
  \\ `?idx. find_in_bucket p b = SOME idx` by
       metis_tac[find_in_bucket_MEM_imp_SOME]
  \\ pop_assum strip_assume_tac
  \\ qexists_tac `idx`
  \\ fs[ph_ok_def]
  \\ simp[ph_lookup_state_def]
  \\ simp[]
QED

(* A convenient “subset” view of `ph_all_present_state`: it is true exactly when
   every element in the query list is present in the abstract set model. *)
Theorem ph_all_present_state_iff_subset:
  !ps s.
    ph_invariant s ==>
      (ph_all_present_state ps s <=>
        !p. MEM p ps ==> p IN ph_set s)
Proof
  Induct_on `ps`
  \\ rpt strip_tac
  >- simp[ph_all_present_state_def]
  \\ eq_tac
  >- (
    rpt strip_tac
    \\ fs[ph_invariant_def]
    \\ `!q. MEM q (h::ps) ==> q IN ph_set s` by
         (match_mp_tac ph_all_present_state_sound \\ simp[])
    \\ first_x_assum (qspec_then `p` mp_tac)
    \\ simp[] )
  \\ rpt strip_tac
  \\ `h IN ph_set s` by (first_x_assum match_mp_tac \\ simp[])
  \\ `MEM h s.elems` by fs[ph_set_def]
  \\ `?idx. ph_lookup_state h s = SOME idx` by metis_tac[ph_lookup_state_complete]
  \\ simp[ph_all_present_state_def]
  \\ first_x_assum match_mp_tac
  \\ rpt strip_tac
  \\ first_x_assum match_mp_tac
  \\ simp[]
QED

(* A stronger “pointwise” completeness statement that is often easier to use in
   refinement proofs: if `p` is in the abstract set, then lookup succeeds. *)
Theorem ph_lookup_state_complete_wrt_set:
  !p s. ph_invariant s /\ p IN ph_set s ==> ?idx. ph_lookup_state p s = SOME idx
Proof
  rw[ph_set_def]
  \\ metis_tac[ph_lookup_state_complete]
QED

(* ------------------------------------------------------------------------- *)
(*  Translator/refinement goals (informal placeholders for later stages)       *)
(* ------------------------------------------------------------------------- *)

(* Ultimately we will want to connect the CakeML-translated monadic operations
   (`ph_create`, `ph_match`, `ph_all_present`, ...) to the pure-state model
   (`ph_match_state`, `ph_lookup_state`, `ph_all_present_state`).

   The “right” statements here depend on how we choose to represent and relate
   the global-state translator configuration to `ph_state`.  For now we record
   just the existence of some relation `R` that makes the operations correspond. *)

val _ = export_theory ();
