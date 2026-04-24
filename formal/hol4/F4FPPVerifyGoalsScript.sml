(*
  File: formal/hol4/F4FPPVerifyGoalsScript.sml

  Purpose
  - Collect the top-level *specification statements* we ultimately want for the
    equivalence of the “slow” and “fast” F4/FPP verifiers.
  - This is intentionally top-down: we introduce abstract interfaces for the
    two concrete programs (domain enumeration + fast-set construction), and
    state the key lemmas that will connect them to the abstract set semantics
    in `F4FPPVerifySpecTheory`.

  How to read this theory
  - Think of `dom_list g` as the triple-list enumerated by the slow checker
    (`SimplerVerifyF4FPP.sml`), and `fast_list g` as the list of parameters
    accumulated by the fast checker (`VerifyF4FPP.sml`).
  - The concrete SML/C++/Atlas work will refine these abstract constants into
    definitions and then discharge the assumptions recorded here.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyAlgTheory;
open F4FPPVerifyFastTheory;

val _ = new_theory "F4FPPVerifyGoals";

(* Abstract “program outputs” to be refined/connected later. *)
val _ = new_constant ("dom_list", ``:group -> triple list``);
val _ = new_constant ("fast_list", ``:group -> param list``);

Definition U_fast_def:
  U_fast g = set (fast_list g)
End

Definition Dom_def:
  Dom g = set (dom_list g)
End

(* What the slow checker computes, abstractly: it checks there are no missing
   witnesses in the domain list relative to `U_fast`. *)
Definition slow_ok_def:
  slow_ok g <=>
    (check_domain_fun g (U_fast g) (dom_list g) = 0)
End

(* The key refinement obligation for the slow checker: its list enumerates
   exactly the intended set-level domain. *)
Definition dom_list_correct_def:
  dom_list_correct g <=>
    Dom g = D_slow g
End

(* The key refinement obligation for the fast checker: it is sound w.r.t. the
   intended domain (i.e. every fast element is witnessed by a triple). *)
Definition fast_sound_def:
  fast_sound g <=>
    sound_wrt_domain g (D_slow g) (U_fast g)
End

(* --- Top-level goals as theorems (some require only earlier theories) --- *)

Theorem slow_ok_imp_complete_rel:
  !g.
    dom_list_correct g /\ slow_ok g ==>
      complete_rel g (D_slow g) (U_fast g)
Proof
  rw[dom_list_correct_def, slow_ok_def, Dom_def]
  \\ `complete_rel g (set (dom_list g)) (U_fast g)` by
       metis_tac[check_domain_fun_eq0_imp_complete_rel_list]
  \\ pop_assum
       (fn cr =>
         qpat_x_assum `set (dom_list g) = D_slow g`
           (fn eq => mp_tac (REWRITE_RULE[eq] cr)))
  \\ simp[]
QED

Theorem slow_ok_and_fast_sound_gives_set_equality:
  !g.
    dom_list_correct g /\ fast_sound g /\ slow_ok g ==>
      U_slow g (D_slow g) = U_fast g
Proof
  rw[fast_sound_def]
  \\ match_mp_tac sound_and_complete_gives_equality
  \\ conj_tac >- simp[]
  \\ metis_tac[slow_ok_imp_complete_rel]
QED

(* The final “program equivalence shape” we are aiming for:
   if the fast checker says OK, then the fast set equals the slow semantics.

   (Later this will be connected to the actual SML `run()` outputs, and to a
   statement that the slow checker is a faithful test of completeness.) *)
Theorem fast_ok_implies_correctness_goal:
  !g.
    dom_list_correct g /\ fast_sound g /\ slow_ok g ==>
      (!pi. pi IN U_slow g (D_slow g) <=> pi IN U_fast g)
Proof
  rw[]
  \\ `U_slow g (D_slow g) = U_fast g` by metis_tac[slow_ok_and_fast_sound_gives_set_equality]
  \\ simp[]
QED

val _ = export_theory ();
