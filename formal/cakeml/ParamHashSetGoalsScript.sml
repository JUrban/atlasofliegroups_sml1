(*
  File: formal/cakeml/ParamHashSetGoalsScript.sml

  Purpose
  - Provide a clean “set interface” view of the ParamHash pure-state model from
    `ParamHashProgTheory`, suitable for refinement into the HOL4 obligations in
    `formal/hol4/F4FPPVerifyParamHashBridgeDecomposeGoalsTheory`.

  Key idea
  - The fast SML code exposes a mutable ParamHash through two observations:
      - `list() : param list`
      - `contains : param -> bool`
    The HOL4 development factors correctness into:
      - representation correctness (contains agrees with list-membership), and
      - agreement of the stored set with the semantic target set (`U_fast`).

  What this file contributes
  - For the CakeML/monadic proof path, we isolate the *data-structure* side:
      under `ph_invariant s`, the pure-state lookup-based predicate
        `ph_contains_state p s`
      is equivalent to membership of `p` in the abstract set `ph_set s`,
      and hence equivalent to `MEM p s.elems`.

  Status
  - Theorems in this file are proved (no `cheat`), and are intended to be
    re-used when connecting the monadic translator semantics to `ph_state`.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;
open listTheory;

open ParamHashProgTheory;
open ParamHashGoalsTheory;

val _ = new_theory "ParamHashSetGoals";

(* A pure-state membership test, matching the observable behaviour of
   `ParamHash.contains` (up to translation/FFI details). *)
Definition ph_contains_state_def:
  ph_contains_state (p:num) (s:ph_state) <=>
    ?idx. ph_lookup_state p s = SOME idx
End

Theorem ph_contains_state_sound_wrt_set:
  !p s. ph_invariant s /\ ph_contains_state p s ==> p IN ph_set s
Proof
  rw[ph_contains_state_def]
  \\ fs[ph_invariant_def]
  \\ metis_tac[ph_lookup_state_SOME_imp_in_set]
QED

Theorem ph_contains_state_complete_wrt_set:
  !p s. ph_invariant s /\ p IN ph_set s ==> ph_contains_state p s
Proof
  rw[ph_contains_state_def]
  \\ metis_tac[ph_lookup_state_complete_wrt_set]
QED

Theorem ph_contains_state_iff_set:
  !p s. ph_invariant s ==> (ph_contains_state p s <=> p IN ph_set s)
Proof
  metis_tac[ph_contains_state_sound_wrt_set, ph_contains_state_complete_wrt_set]
QED

(* A “representation correctness” view: contains agrees with membership in the
   enumerated list `s.elems`. (Duplicates do not matter because HOL4’s `MEM`
   is defined via `LIST_TO_SET`.) *)
Theorem ph_rep_ok_iff_MEM_elems:
  !p s. ph_invariant s ==> (ph_contains_state p s <=> MEM p s.elems)
Proof
  rw[ph_contains_state_iff_set]
  \\ simp[ph_set_def]
QED

(* Trivial list/set correspondence (useful when aligning with HOL4’s
   `paramhash_stores_U_fast` goal). *)
Theorem ph_set_is_set_of_elems:
  !s. ph_set s = set (s.elems)
Proof
  simp[ph_set_def]
QED

val _ = export_theory ();

