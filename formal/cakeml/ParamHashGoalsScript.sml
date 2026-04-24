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
  cheat
QED

(* A convenient “subset” view of `ph_all_present_state`: it is true exactly when
   every element in the query list is present in the abstract set model. *)
Theorem ph_all_present_state_iff_subset:
  !ps s.
    ph_invariant s ==>
      (ph_all_present_state ps s <=>
        !p. MEM p ps ==> p IN ph_set s)
Proof
  cheat
QED

(* A stronger “pointwise” completeness statement that is often easier to use in
   refinement proofs: if `p` is in the abstract set, then lookup succeeds. *)
Theorem ph_lookup_state_complete_wrt_set:
  !p s. ph_invariant s /\ p IN ph_set s ==> ?idx. ph_lookup_state p s = SOME idx
Proof
  cheat
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

