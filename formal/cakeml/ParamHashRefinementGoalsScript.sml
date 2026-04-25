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
  - The theorems here are stated precisely but are currently `cheat`ed.
    This is deliberate: the user value is in pinning down the exact “glue”
    statements that will replace the HOL4-side `*Cheats*` bridge lemmas.

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

open ml_monad_translatorTheory;  (* for `M_success` / `M_failure` *)

open ParamHashProgTheory;
open ParamHashInvariantGoalsTheory;
open ParamHashSetGoalsTheory;

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
  (*
    Intended proof outline (later):
    - unfold `ph_lookup_def` and show the `bucket_count = 0` and Subscript
      cases are impossible under `ph_ok`,
    - relate `buckets_sub (bucket_index p m)` to `EL i s.buckets`.
  *)
  cheat
QED

Theorem ph_contains_refines_contains_state:
  !p s.
    ph_ok s ==>
      ph_contains p s =
        (M_success (ph_contains_state p s), s)
Proof
  (*
    Intended proof outline (later):
    - unfold `ph_contains_def`, rewrite via `ph_lookup_refines_lookup_state`,
    - rewrite `ph_contains_state_def`.
  *)
  cheat
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
  (*
    Intended proof outline (later):
    - case split on `ph_lookup_state p s` using `ph_lookup_refines_lookup_state`,
    - in the NONE branch, show the `update_buckets` and `set_elems` effects
      match `ph_match_state_def`’s `LUPDATE`/append, and show no exceptions.
  *)
  cheat
QED

Theorem ph_insert_all_refines_build_state:
  !ps s.
    ph_ok s ==>
      ph_insert_all ps s =
        (M_success (), ph_build_state ps s)
Proof
  (*
    Intended proof outline (later):
    - induction on `ps`,
    - use `ph_match_refines_match_state` to rewrite the monadic step.
  *)
  cheat
QED

Theorem ph_all_present_refines_all_present_state:
  !ps s.
    ph_ok s ==>
      ph_all_present ps s =
        (M_success (ph_all_present_state ps s), s)
Proof
  (*
    Intended proof outline (later):
    - induction on `ps`,
    - rewrite via `ph_contains_refines_contains_state`,
    - note `ph_all_present` does not mutate state.
  *)
  cheat
QED

(* A convenient corollary: if we build from an invariant state, then the
   resulting table’s observable `contains` agrees with membership in `elems`. *)
Theorem ph_contains_after_insert_all_iff_MEM_elems:
  !p ps s s'.
    ph_invariant s /\
    ph_insert_all ps s = (M_success (), s') ==>
      (ph_contains_state p s' <=> MEM p s'.elems)
Proof
  rpt strip_tac
  \\ `ph_ok s` by fs[ph_invariant_def]
  \\ `ph_insert_all ps s = (M_success (), ph_build_state ps s)` by
       metis_tac[ph_insert_all_refines_build_state]
  \\ `s' = ph_build_state ps s` by metis_tac[pairTheory.PAIR_EQ]
  \\ fs[]
  \\ match_mp_tac ph_rep_ok_iff_MEM_elems
  \\ metis_tac[ph_build_state_preserves_invariant]
QED

val _ = export_theory ();
