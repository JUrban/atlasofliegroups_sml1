(*
  File: formal/hol4/F4FPPVerifyFastScript.sml

  Purpose
  - Express the top-level “equivalence shape” from `VERIFY_ESTIMATE.md`:
      - a soundness direction: `U_fast ⊆ U_slow` (every fast element is witnessed
        by the slow semantics/domain), and
      - a completeness direction: `U_slow ⊆ U_fast` (what the slow checker tries
        to establish).

  This theory does not attempt to model Atlas/C++ semantics; it just packages
  the logical structure of the argument we want to build up to.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;

val _ = new_theory "F4FPPVerifyFast";

Definition sound_wrt_domain_def:
  sound_wrt_domain (g:group) (dom:triple set) (U_fast:param set) <=>
    !pi. pi IN U_fast ==> ?t. t IN dom /\ first_final_term (mk_param g t) = SOME pi /\ is_unitary pi
End

Theorem sound_and_complete_gives_equality:
  !g dom U_fast.
    sound_wrt_domain g dom U_fast /\ complete_rel g dom U_fast ==>
    U_slow g dom = U_fast
Proof
  rw [sound_wrt_domain_def] >>
  (* show both inclusions *)
  rw [EXTENSION, U_slow_def] >>
  metis_tac [complete_rel_def]
QED

val _ = export_theory ();

