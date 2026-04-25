(*
  File: formal/cakeml/ParamHashRefinementGoalsScript.sml

  Purpose
  - Make explicit the next layer of the CakeML proof plan for the fast checker:
    relate the monadic translator-level operations (`ph_match`, `ph_insert_all`,
    `ph_all_present`, ...) from `ParamHashProgTheory` to the *pure-state*
    functional model (`ph_match_state`, `ph_build_state`, `ph_all_present_state`).

  Why this file exists
  - `ParamHashSetGoalsTheory` already proves (no `cheat`) the *extensional*
    data-structure lemma we ultimately need:

      under `ph_invariant s`,
        `ph_contains_state p s <=> MEM p s.elems`.

  - To connect this to a CakeML verification story, we still need to show that
    executing the translated monadic code (which uses refs + resizable arrays)
    corresponds to the pure-state model.  The key step is to prove/record
    refinement theorems of the form:

      (monadic operation on state `s`) = (M_success result, new_state)
      where `(result,new_state)` is exactly the pure-state function’s output.

  Status
  - Most refinement theorems in this file are proved by unfolding definitions.
  - `ph_match_state_preserves_ok` is proved (no `cheat`) in the shared pure
    base theory `ParamHashBuildGoalsTheory`; it is used here to justify the
    `insert_all`/`build_state` iteration step.
  - “End-to-end” consequences that rely on invariant-preservation (which is
    still cheat-tainted) are stated in `ParamHashEndToEndGoalsTheory` instead.

  Planned discharge
  - Prove these theorems by:
      - unfolding the monadic definitions from `ParamHashProgTheory`,
      - using the safety consequences of `ph_ok`/`ph_invariant`
        (bucket_index bounds, array length, etc.),
      - and performing straightforward list/array update reasoning.

  Relation to HOL4-side obligations
  - Once proved, these lemmas are intended to justify the bridge goals in:
      `formal/hol4/F4FPPVerifyParamHashBridgeStateRefineCheatsGoalsScript.sml`
      `formal/hol4/F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoalsScript.sml`
    by providing a concrete “state witness” for `paramhash_observes_state`.
*)

open HolKernel Parse boolLib bossLib;

open listTheory;
open pairTheory;
open pred_setTheory pred_setLib;

open ml_monadBaseTheory;
	open ml_monad_translatorTheory;  (* for `M_success` / `M_failure` *)

	open ParamHashProgTheory;
	open ParamHashSetGoalsTheory;
	open ParamHashBuildGoalsTheory;

val _ = new_theory "ParamHashRefinementGoals";

(* ------------------------------------------------------------------------- *)
(*  Local helpers                                                            *)
(* ------------------------------------------------------------------------- *)

(* A “success only” spec for `ph_lookup`: under `ph_ok` it should never raise
   and should agree with the pure-state `ph_lookup_state`. *)
Theorem ph_lookup_refines_lookup_state:
  !p s.
    ph_ok s ==>
      ph_lookup p s = (M_success (ph_lookup_state p s), s)
Proof
  rpt strip_tac
  \\ fs[ph_ok_def]
  \\ simp[ph_lookup_def, st_ex_bind_def, st_ex_return_def, get_bucket_count_def]
  \\ simp[buckets_sub_def, Marray_sub_def, st_ex_bind_def, st_ex_return_def]
  \\ `bucket_index p s.bucket_count < s.bucket_count` by metis_tac[bucket_index_lt]
  \\ `bucket_index p s.bucket_count < LENGTH s.buckets` by fs[]
  \\ simp[Msub_eq, ph_lookup_state_def, LET_THM]
QED

Theorem ph_contains_refines_contains_state:
  !p s.
    ph_ok s ==>
      ph_contains p s =
        (M_success (ph_contains_state p s), s)
Proof
  rpt strip_tac
  \\ simp[ph_contains_def, st_ex_bind_def, st_ex_return_def]
  \\ simp[ph_lookup_refines_lookup_state]
  \\ Cases_on `ph_lookup_state p s`
  \\ simp[ph_contains_state_def]
QED

(* ------------------------------------------------------------------------- *)
(*  Main refinement goals                                                     *)
(* ------------------------------------------------------------------------- *)

Theorem ph_match_refines_match_state:
  !p s.
    ph_ok s ==>
      ph_match p s =
        (M_success (FST (ph_match_state p s)), SND (ph_match_state p s))
Proof
  rpt strip_tac
  \\ simp[ph_match_def, st_ex_bind_def, st_ex_ignore_bind_def, st_ex_return_def]
  \\ simp[ph_lookup_refines_lookup_state]
  \\ Cases_on `ph_lookup_state p s`
  >- (
    simp[ph_match_state_def, LET_THM]
    \\ fs[ph_ok_def]
    \\ simp[get_bucket_count_def, get_count_def, get_elems_def,
            buckets_sub_def, update_buckets_def, set_count_def, set_elems_def,
            Marray_sub_def, Marray_update_def,
            st_ex_bind_def, st_ex_ignore_bind_def, st_ex_return_def]
    \\ `bucket_index p s.bucket_count < s.bucket_count` by
         metis_tac[bucket_index_lt]
    \\ `bucket_index p s.bucket_count < LENGTH s.buckets` by fs[]
    \\ simp[Msub_eq, Mupdate_eq, LET_THM, ph_state_component_equality])
  \\ simp[ph_match_state_def]
QED

Theorem ph_insert_all_refines_build_state:
  !ps s.
    ph_ok s ==>
      ph_insert_all ps s =
        (M_success (), ph_build_state ps s)
Proof
  Induct_on `ps`
  >- (
    rpt strip_tac
    \\ simp[ph_insert_all_def, ph_build_state_def, st_ex_return_def])
  \\ rpt strip_tac
  \\ simp[ph_insert_all_def, st_ex_ignore_bind_def, st_ex_bind_def]
  \\ simp[ph_match_refines_match_state]
  \\ `ph_ok (SND (ph_match_state h s))` by
       metis_tac[ph_match_state_preserves_ok]
  \\ first_x_assum (qspec_then `SND (ph_match_state h s)` mp_tac)
  \\ impl_tac >- simp[]
  \\ simp[ph_build_state_def]
QED

Theorem ph_all_present_refines_all_present_state:
  !ps s.
    ph_ok s ==>
      ph_all_present ps s =
        (M_success (ph_all_present_state ps s), s)
Proof
  Induct_on `ps`
  >- (
    rpt strip_tac
    \\ simp[ph_all_present_def, ph_all_present_state_def, st_ex_return_def])
  \\ rpt strip_tac
  \\ simp[ph_all_present_def, st_ex_bind_def, st_ex_return_def]
  \\ simp[ph_contains_refines_contains_state]
  \\ Cases_on `ph_lookup_state h s`
  \\ simp[ph_contains_state_def, ph_all_present_state_def]
QED

val _ = export_theory ();
