(*
  File: formal/hol4/F4FPPVerifyParamHashStateGoalsScript.sml

  Purpose
  - Introduce an explicit *abstract state model* for the mutable ParamHash
    structure and state the key invariant we ultimately want to establish
    about it.
  - Prove that, under this invariant and a simplifying equality assumption
    (`atlas_eq_is_hol_eq`), the observable membership predicate derived from
    lookup is equivalent to membership in the enumerated list of stored
    elements.

  Why this exists
  - The HOL4 development’s fast side currently talks about ParamHash only via
    the observations `paramhash_list g` and `paramhash_contains g`.
  - For verification (CakeML or otherwise) it is useful to have an intermediate
    *state-level* correctness statement:

      “the table’s `contains` agrees with membership in `list()`”.

    This file makes that statement precise for an abstract state `ph_state`
    with buckets + an insertion-order list of stored parameters.

  Scope / status
  - This model is intentionally minimal: it does not yet model resizing, clone
    ownership, or insertion correctness.  It is the attachment point for those
    later refinements.
  - The main theorem here is proved, but it relies on the simplifying contract
    `atlas_eq_is_hol_eq` (recorded in
    `F4FPPVerifyAtlasFFIContractsGoalsTheory`).
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;
open optionTheory;

open F4FPPVerifySpecTheory;
open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyParamHashBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashStateGoals";

(* ------------------------------------------------------------------------- *)
(*  Abstract state model                                                      *)
(* ------------------------------------------------------------------------- *)

Datatype:
  ph_state = <|
    bucket_count : num;
    elems : param list;
    buckets : ((param # num) list) list
  |>
End

Definition bucket_index_def:
  bucket_index (p:param) m = atlas_hash_mod p m
End

Definition find_in_bucket_def:
  (find_in_bucket (p:param) ([]:(param # num) list) = (NONE:num option)) /\
  (find_in_bucket p ((q,idx)::rest) =
     if atlas_eq p q then SOME idx else find_in_bucket p rest)
End

Definition ph_lookup_state_def:
  ph_lookup_state (p:param) (s:ph_state) =
    if s.bucket_count = 0 then NONE
    else
      let i = bucket_index p s.bucket_count in
        find_in_bucket p (EL i s.buckets)
End

Definition ph_contains_state_def:
  ph_contains_state (p:param) (s:ph_state) <=>
    ?idx. ph_lookup_state p s = SOME idx
End

Definition ph_set_def:
  ph_set (s:ph_state) = set s.elems
End

(* A structural “hash-table invariant” analogous to the one used on the CakeML
   side: buckets agree with `bucket_index`, and indices point back into `elems`. *)
Definition ph_ok_def:
  ph_ok (s:ph_state) <=>
    s.bucket_count <> 0 /\
    LENGTH s.buckets = s.bucket_count /\
    (!p idx.
      MEM (p,idx) (FLAT s.buckets) ==>
        idx < LENGTH s.elems /\ EL idx s.elems = p)
End

Definition ph_bucketed_def:
  ph_bucketed (s:ph_state) <=>
    !i p idx.
      i < LENGTH s.buckets /\ MEM (p,idx) (EL i s.buckets) ==>
        bucket_index p s.bucket_count = i
End

Definition ph_covered_def:
  ph_covered (s:ph_state) <=>
    !j. j < LENGTH s.elems ==> MEM (EL j s.elems, j) (FLAT s.buckets)
End

Definition ph_invariant_def:
  ph_invariant (s:ph_state) <=>
    ph_ok s /\ ph_bucketed s /\ ph_covered s
End

(* ------------------------------------------------------------------------- *)
(*  Helper lemmas about `find_in_bucket`                                      *)
(* ------------------------------------------------------------------------- *)

Theorem find_in_bucket_SOME_MEM:
  !p b idx. find_in_bucket p b = SOME idx ==> ?q. MEM (q,idx) b /\ atlas_eq p q
Proof
  Induct_on `b`
  \\ rw[find_in_bucket_def]
  \\ Cases_on `h`
  \\ fs[find_in_bucket_def]
  \\ Cases_on `atlas_eq p q`
  >- (fs[] \\ qexists_tac `q` \\ simp[])
  \\ fs[]
  \\ first_x_assum drule
  \\ strip_tac
  \\ qexists_tac `q'`
  \\ simp[]
QED

Theorem find_in_bucket_MEM_imp_SOME:
  !p b idx.
    atlas_eq_is_hol_eq /\ MEM (p,idx) b ==> ?idx'. find_in_bucket p b = SOME idx'
Proof
  Induct_on `b`
  \\ simp[]
  \\ Cases_on `h`
  \\ simp[find_in_bucket_def]
  \\ strip_tac
  \\ fs[atlas_eq_is_hol_eq_def]
  \\ rw[]
  \\ metis_tac[]
QED

(* ------------------------------------------------------------------------- *)
(*  Main state-level representation correctness theorem                        *)
(* ------------------------------------------------------------------------- *)

Theorem ph_contains_state_iff_MEM_elems:
  !p s.
    atlas_eq_is_hol_eq /\ atlas_hash_range /\ ph_invariant s ==>
      (ph_contains_state p s <=> MEM p s.elems)
Proof
  (*
    Intended proof (later, without `cheat`):
    - expand `ph_invariant` into `ph_ok`, `ph_bucketed`, `ph_covered`,
    - (⇒) from `ph_lookup_state p s = SOME idx`, use `find_in_bucket_SOME_MEM`
      to get a bucket entry `(q,idx)` with `atlas_eq p q`; then use `ph_ok` and
      `atlas_eq_is_hol_eq` to conclude `EL idx elems = p`, hence `MEM p elems`,
    - (⇐) from `MEM p elems`, pick `j` with `EL j elems = p`; use `ph_covered`
      to find `(p,j)` in some bucket; use `ph_bucketed` to identify the right
      bucket index for lookup, and `find_in_bucket_MEM_imp_SOME` to conclude
      `ph_lookup_state p s = SOME idx'` for some `idx'`.
  *)
  cheat
QED

(* ------------------------------------------------------------------------- *)
(*  Connecting the state model to the ParamHash bridge obligations             *)
(* ------------------------------------------------------------------------- *)

Definition paramhash_observes_state_def:
  paramhash_observes_state g (s:ph_state) <=>
    paramhash_list g = s.elems /\
    (!p. paramhash_contains g p <=> ph_contains_state p s)
End

Theorem paramhash_observes_state_and_invariant_imp_paramhash_rep_ok:
  !g s.
    atlas_eq_is_hol_eq /\ atlas_hash_range /\ ph_invariant s /\ paramhash_observes_state g s ==>
      paramhash_rep_ok g
Proof
  rw[paramhash_rep_ok_def, paramhash_observes_state_def]
  \\ metis_tac[ph_contains_state_iff_MEM_elems]
QED

val _ = export_theory ();
