(*
  File: formal/hol4/F4FPPVerifySpecAtlasEqGoalsScript.sml

  Purpose
  - Introduce a “modulo Atlas equality” version of the slow/fast set-equivalence
    specification from `F4FPPVerifySpecTheory`.
  - The main deliverable is a *top-down* statement of what we ultimately want
    without assuming `atlas_eq_is_hol_eq`:

      `set_atlas_eq U_fast (U_slow g dom)`

    where `set_atlas_eq` is extensional equality of sets up to the semantic
    equality `atlas_eq`.

  Motivation
  - The fast computation and the slow brute-force search can legitimately
    produce different representatives of the same semantic Atlas parameter.
  - Therefore, the end-to-end “equivalence” theorem we ultimately want should
    be phrased modulo `atlas_eq`, not HOL equality on `param`.

  Status
  - This theory is “OK”: it is purely logical and does not introduce new
    `cheat`s.  The hard work is discharged by proving/assuming the two
    directional obligations that connect the concrete programs to the abstract
    sets:
      - a modulo-`atlas_eq` fast soundness/witness condition, and
      - a modulo-`atlas_eq` slow completeness condition.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;

val _ = new_theory "F4FPPVerifySpecAtlasEqGoals";

(* Slow completeness, phrased modulo `atlas_eq`: every slow-witnessed unitary
   final term is *represented up to `atlas_eq`* in the fast set. *)
Definition complete_rel_atlas_eq_def:
  complete_rel_atlas_eq (g:group) (dom:triple set) (U_fast:param set) <=>
    !t pi.
      t IN dom /\
      first_final_term (mk_param g t) = SOME pi /\
      is_unitary pi ==>
        mem_set_atlas_eq pi U_fast
End

(* A “missing witness” predicate, modulo `atlas_eq`: a triple witnesses a unitary
   final term that is not represented (up to `atlas_eq`) in the fast set. *)
Definition missing_witness_atlas_eq_def:
  missing_witness_atlas_eq (g:group) (U_fast:param set) (t:triple) <=>
    ?pi.
      first_final_term (mk_param g t) = SOME pi /\
      is_unitary pi /\
      ~mem_set_atlas_eq pi U_fast
End

(* Fast soundness/witnessing, phrased modulo `atlas_eq`: every fast element is
   semantically equal to a slow-witnessed unitary final term. *)
Definition sound_wrt_domain_atlas_eq_def:
  sound_wrt_domain_atlas_eq (g:group) (dom:triple set) (U_fast:param set) <=>
    !pi.
      pi IN U_fast ==>
        ?t pi'.
          t IN dom /\
          first_final_term (mk_param g t) = SOME pi' /\
          is_unitary pi' /\
          atlas_eq pi pi'
End

Theorem complete_rel_atlas_eq_imp_slow_mem_mod_fast:
  !g dom U_fast.
    complete_rel_atlas_eq g dom U_fast ==>
      !pi. pi IN U_slow g dom ==> mem_set_atlas_eq pi U_fast
Proof
  rw[complete_rel_atlas_eq_def, U_slow_def]
QED

Theorem complete_rel_atlas_eq_iff_no_missing:
  !g dom U_fast.
    complete_rel_atlas_eq g dom U_fast <=>
      !t. t IN dom ==> ~missing_witness_atlas_eq g U_fast t
Proof
  rw[complete_rel_atlas_eq_def, missing_witness_atlas_eq_def]
  \\ metis_tac[]
QED

Theorem sound_wrt_domain_atlas_eq_imp_fast_mem_mod_slow:
  !g dom U_fast.
    sound_wrt_domain_atlas_eq g dom U_fast ==>
      !pi. pi IN U_fast ==> mem_set_atlas_eq pi (U_slow g dom)
Proof
  rw[sound_wrt_domain_atlas_eq_def, U_slow_def, mem_set_atlas_eq_def]
  \\ metis_tac[]
QED

(* Generic closure reasoning: if every element of each set is represented (up to
   `atlas_eq`) in the other, then the sets are extensionally equal modulo
   `atlas_eq`. *)
Theorem sound_and_complete_mod_atlas_eq_gives_set_atlas_eq:
  !U V.
    atlas_eq_equiv /\
    (!x. x IN U ==> mem_set_atlas_eq x V) /\
    (!y. y IN V ==> mem_set_atlas_eq y U) ==>
      set_atlas_eq U V
Proof
  rw[atlas_eq_equiv_def, set_atlas_eq_def]
  \\ EQ_TAC
  >- (
    rw[mem_set_atlas_eq_def]
    \\ first_x_assum drule
    \\ rw[mem_set_atlas_eq_def]
    \\ qexists_tac `q'`
    \\ metis_tac[])
  \\ (
    rw[mem_set_atlas_eq_def]
    \\ first_x_assum drule
    \\ rw[mem_set_atlas_eq_def]
    \\ qexists_tac `q'`
    \\ metis_tac[])
QED

(* Main spec-level goal: fast soundness (modulo) + slow completeness (modulo)
   gives set equality modulo `atlas_eq`. *)
Theorem sound_and_complete_atlas_eq_gives_set_atlas_eq:
  !g dom U_fast.
    atlas_eq_equiv /\
    sound_wrt_domain_atlas_eq g dom U_fast /\
    complete_rel_atlas_eq g dom U_fast ==>
      set_atlas_eq U_fast (U_slow g dom)
Proof
  rpt strip_tac
  \\ match_mp_tac sound_and_complete_mod_atlas_eq_gives_set_atlas_eq
  \\ conj_tac >- fs[]
  \\ conj_tac
  >- metis_tac[sound_wrt_domain_atlas_eq_imp_fast_mem_mod_slow]
  \\ metis_tac[complete_rel_atlas_eq_imp_slow_mem_mod_fast]
QED

val _ = export_theory ();
