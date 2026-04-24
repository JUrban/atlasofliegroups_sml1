(*
  File: formal/hol4/F4FPPVerifyGoalsAtlasEqScript.sml

  Purpose
  - Restate the “fast/slow agree” conclusion in a form that is robust to
    representation choices, by using `set_atlas_eq` (set equality modulo
    Atlas semantic equality `atlas_eq`).

  Status
  - This file is intentionally lightweight: it does not yet change the
    underlying fast/slow obligations to be modulo-`atlas_eq`.
  - Instead, it provides a compatibility lemma showing that the existing
    HOL-level set equality result implies `set_atlas_eq`, giving us a clean
    target statement to strengthen towards as we reduce reliance on
    `atlas_eq_is_hol_eq`.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPVerifySpecTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;

val _ = new_theory "F4FPPVerifyGoalsAtlasEq";

Theorem eq_imp_set_atlas_eq:
  !U V. (U = V) ==> set_atlas_eq U V
Proof
  rw[set_atlas_eq_def, mem_set_atlas_eq_def]
QED

Theorem slow_ok_and_fast_sound_gives_set_atlas_eq:
  !g.
    dom_list_correct g /\ fast_sound g /\ slow_ok g ==>
      set_atlas_eq (U_slow g (D_slow g)) (U_fast g)
Proof
  rpt gen_tac
  \\ strip_tac
  \\ `U_slow g (D_slow g) = U_fast g` by
       metis_tac[slow_ok_and_fast_sound_gives_set_equality]
  \\ metis_tac[eq_imp_set_atlas_eq]
QED

val _ = export_theory ();

