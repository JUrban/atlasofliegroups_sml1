(*
  File: formal/hol4/F4FPPVerifySpecScript.sml

  Purpose
  - First formalization step from `VERIFY_ESTIMATE.md` (Stage A / Step 1):
    define abstract set semantics for the “slow” completeness checker and
    prove the basic subset lemma it is intended to establish.

  Scope
  - This theory is *assumption-heavy* by design: Atlas/C++ semantics are left
    abstract via uninterpreted constants (`mk_param`, `first_final_term`,
    `is_unitary`).
  - The point is to nail down the statement we ultimately want, and provide a
    place to attach progressively stronger assumptions/refinement arguments.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;
open optionTheory;

val _ = new_theory "F4FPPVerifySpec";

val _ = new_type ("group", 0);
val _ = new_type ("ratvec", 0);
val _ = new_type ("param", 0);

val _ = Datatype `
  triple = <| x : num ; lambda : ratvec ; gamma : ratvec |>`;

val _ = new_constant ("mk_param", ``:group -> triple -> param``);
val _ = new_constant ("first_final_term", ``:param -> param option``);
val _ = new_constant ("is_unitary", ``:param -> bool``);

Definition U_slow_def:
  U_slow (g:group) (dom:triple set) : param set =
    {pi | ?t. t IN dom /\ first_final_term (mk_param g t) = SOME pi /\ is_unitary pi}
End

Definition complete_rel_def:
  complete_rel (g:group) (dom:triple set) (U_fast:param set) <=>
    !t pi. t IN dom /\ first_final_term (mk_param g t) = SOME pi /\ is_unitary pi ==> pi IN U_fast
End

Theorem complete_rel_imp_subset:
  !g dom U_fast. complete_rel g dom U_fast ==> U_slow g dom SUBSET U_fast
Proof
  rw [complete_rel_def, U_slow_def, SUBSET_DEF] >>
  metis_tac []
QED

val _ = export_theory ();
