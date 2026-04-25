(*
  File: formal/cakeml/ParamHashEndToEndGoalsScript.sml

  Purpose
  - Provide an explicit “end-to-end” specification layer for the CakeML
    ParamHash model: `create; insert_all; then query`.

  Relation to the overall F4/FPP verification story
  - On the HOL4 side we want to ultimately discharge obligations of the form:
      - the concrete ParamHash `contains` observation is complete/sound, and
      - the stored set corresponds to the intended semantic set `U_fast`.
  - The CakeML plan is to prove that the algorithmic skeleton of ParamHash
    (bucketing, insertion, lookup) refines a pure-state model.
  - This theory packages the *composition* of the previously-stated goals:
      - initialization (`ParamHashCreateGoalsTheory`),
      - pure-state invariant preservation (`ParamHashInvariantGoalsTheory`),
      - extensional set-view (`ParamHashSetGoalsTheory`),
      - and monadic ⇔ pure-state refinement goals (`ParamHashRefinementGoalsTheory`).

  Status
  - Pure-state consequences are proved (OK).
  - The top-level theorem
      `ph_build_into_new_refines_build_from_create_state`
    is proved by *composition* (no `cheat`).
*)

open HolKernel Parse boolLib bossLib;

open listTheory;
open pairTheory;
open pred_setTheory pred_setLib;

open ml_monadBaseTheory;
open ml_monad_translatorTheory;  (* `M_success` / `M_failure` *)

open ParamHashProgTheory;
open ParamHashGoalsTheory;
open ParamHashSetGoalsTheory;
open ParamHashInvariantGoalsTheory;
open ParamHashRefinementGoalsTheory;
open ParamHashCreateGoalsTheory;

val _ = new_theory "ParamHashEndToEndGoals";

(* ------------------------------------------------------------------------- *)
(*  Pure end-to-end reference model                                           *)
(* ------------------------------------------------------------------------- *)

Definition ph_build_from_create_state_def:
  ph_build_from_create_state m ps =
    ph_build_state ps (ph_create_state m)
End

Theorem ph_build_from_create_state_invariant:
  !m ps. m <> 0n ==> ph_invariant (ph_build_from_create_state m ps)
Proof
  rpt strip_tac
  \\ simp[ph_build_from_create_state_def]
  \\ match_mp_tac ph_build_state_preserves_invariant
  \\ match_mp_tac ph_create_state_invariant
  \\ simp[]
QED

Theorem ph_contains_state_build_from_create_iff_MEM_elems:
  !p m ps.
    m <> 0n ==>
      (ph_contains_state p (ph_build_from_create_state m ps) <=>
       MEM p (ph_build_from_create_state m ps).elems)
Proof
  rpt strip_tac
  \\ match_mp_tac ph_rep_ok_iff_MEM_elems
  \\ match_mp_tac ph_build_from_create_state_invariant
  \\ simp[]
QED

Theorem ph_all_present_state_build_from_create_iff_subset:
  !qs m ps.
    m <> 0n ==>
      (ph_all_present_state qs (ph_build_from_create_state m ps) <=>
        !p. MEM p qs ==> p IN ph_set (ph_build_from_create_state m ps))
Proof
  rpt strip_tac
  \\ match_mp_tac ph_all_present_state_iff_subset
  \\ match_mp_tac ph_build_from_create_state_invariant
  \\ simp[]
QED

(* ------------------------------------------------------------------------- *)
(*  Monadic “create; insert_all” program and its desired refinement theorem   *)
(* ------------------------------------------------------------------------- *)

Definition ph_build_into_new_def:
  ph_build_into_new m ps =
    st_ex_ignore_bind (ph_create m) (ph_insert_all ps)
End

Theorem ph_build_into_new_refines_build_from_create_state:
  !m ps s.
    m <> 0n ==>
      ph_build_into_new m ps s = (M_success (), ph_build_from_create_state m ps)
Proof
  rpt strip_tac
  \\ `ph_ok (ph_create_state m)` by metis_tac[ph_create_state_ok]
  \\ fs[ph_build_into_new_def, st_ex_ignore_bind_def]
  \\ fs[ph_create_refines_create_state]
  \\ fs[ph_insert_all_refines_build_state, ph_build_from_create_state_def]
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
