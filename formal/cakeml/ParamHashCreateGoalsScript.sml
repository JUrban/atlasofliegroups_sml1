(*
  File: formal/cakeml/ParamHashCreateGoalsScript.sml

  Purpose
  - Specify the “initialization” part of the CakeML ParamHash story:
      - a pure-state initial table `ph_create_state m`, and
      - refinement-style goals relating the monadic translator-level `ph_create`
        from `ParamHashProgTheory` to that pure initial state.

  Why this matters
  - The later refinement goals in `ParamHashRefinementGoalsTheory` assume a
    well-formed starting state (`ph_ok` / `ph_invariant`) so that:
      - lookups do not raise exceptions, and
      - array indices are in-bounds.
  - The fast verifier’s ParamHash is always built by:
      `create m; insert_all ps; ...`
    so the verification pipeline needs a clear “create gives invariant state”
    lemma to kick off induction/refinement proofs.

  Status
  - The pure-state facts about `ph_create_state` are proved (OK).
  - The refinement theorem connecting monadic `ph_create` to `ph_create_state`
    is currently recorded and `cheat`ed: it depends on translator semantics
    for refs/resizable-arrays and is intended to be discharged later.
*)

open HolKernel Parse boolLib bossLib;

open listTheory;
open rich_listTheory;
open pairTheory;
open pred_setTheory pred_setLib;

open ml_monad_translatorTheory;  (* `M_success` / `M_failure` *)

open ParamHashProgTheory;

val _ = new_theory "ParamHashCreateGoals";

(* ------------------------------------------------------------------------- *)
(*  Pure-state “create” model                                                 *)
(* ------------------------------------------------------------------------- *)

Definition ph_create_state_def:
  ph_create_state (m:num) : ph_state =
    <| bucket_count := m
     ; count := 0n
     ; elems := ([]:num list)
     ; buckets := REPLICATE m ([]:(num # num) list)
     |>
End

Theorem FLAT_REPLICATE_NIL:
  !m. FLAT (REPLICATE m ([]:'a list)) = []
Proof
  Induct
  \\ simp[]
QED

Theorem ph_create_state_ok:
  !m. m <> 0n ==> ph_ok (ph_create_state m)
Proof
  rpt strip_tac
  \\ simp[ph_ok_def, ph_create_state_def, FLAT_REPLICATE_NIL]
QED

Theorem ph_create_state_invariant:
  !m. m <> 0n ==> ph_invariant (ph_create_state m)
Proof
  rpt strip_tac
  \\ simp[ph_invariant_def]
  \\ conj_tac
  >- (match_mp_tac ph_create_state_ok \\ simp[])
  \\ conj_tac
  >- (
    rw[ph_bucketed_def, ph_create_state_def]
    \\ `EL i (REPLICATE m ([]:(num # num) list)) = []` by
         (simp[EL_REPLICATE, LENGTH_REPLICATE] )
    \\ fs[] )
  \\ simp[ph_covered_def, ph_create_state_def]
QED

Theorem ph_lookup_state_create_state_NONE:
  !p m. m <> 0n ==> ph_lookup_state p (ph_create_state m) = NONE
Proof
  rpt strip_tac
  \\ simp[ph_lookup_state_def, ph_create_state_def, LET_THM]
  \\ `bucket_index p m < m` by metis_tac[bucket_index_lt]
  \\ simp[EL_REPLICATE, find_in_bucket_def]
QED

Theorem ph_set_create_state_empty:
  !m. ph_set (ph_create_state m) = ({}:num set)
Proof
  simp[ph_set_def, ph_create_state_def]
QED

(* ------------------------------------------------------------------------- *)
(*  Refinement goals: monadic `ph_create` corresponds to `ph_create_state`     *)
(* ------------------------------------------------------------------------- *)

Theorem ph_create_refines_create_state:
  !m s.
    m <> 0n ==>
      ph_create m s = (M_success (), ph_create_state m)
Proof
  (*
    Intended proof outline (later, without `cheat`):
    - unfold `ph_create_def`,
    - show the success branch performs the four updates:
        set_bucket_count; set_count; set_elems; alloc_buckets
      and that they result in exactly `ph_create_state m`,
    - discharge any translator-side side-conditions about resizeable arrays.
  *)
  cheat
QED

Theorem ph_create_succeeds_imp_invariant:
  !m s s'.
    m <> 0n /\ ph_create m s = (M_success (), s') ==> ph_invariant s'
Proof
  rpt strip_tac
  \\ `s' = ph_create_state m` by metis_tac[ph_create_refines_create_state, PAIR_EQ]
  \\ fs[]
  \\ metis_tac[ph_create_state_invariant]
QED

val _ = export_theory ();
