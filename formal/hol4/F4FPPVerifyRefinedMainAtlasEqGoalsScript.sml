(*
  File: formal/hol4/F4FPPVerifyRefinedMainAtlasEqGoalsScript.sml

  Purpose
  - Provide a refined-main theorem phrased in the *right* end-to-end notion of
    equivalence: set equality modulo the Atlas semantic equality `atlas_eq`.

  Key statement
  - Under:
      - slow-side refinement obligations (component lists are correct),
      - slow-side “no misses” check computed by the list-fold skeleton modulo
        `atlas_eq` (`slow_ok_components_atlas_eq`),
      - fast-side semantic obligation (`fast_semantic_ok`), and
      - bottom-layer total correctness on `U_fast`,
    we can conclude:

      `set_atlas_eq (U_fast g) (U_slow g (D_slow g))`.

  Status
  - “OK”: this theory is purely logical composition. It does not attempt to
    connect these predicates to concrete Poly/ML execution or Atlas/C++.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyFastTheory;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;
open F4FPPVerifySpecAtlasEqGoalsTheory;

	open F4FPPVerifySlowRefineAtlasEqGoalsTheory;
	open F4FPPVerifyFastRefineAtlasEqGoalsTheory;
	open F4FPPVerifyRefinedMainGoalsTheory;
	open F4FPPBottomLayerGoalsAtlasEqTheory;

val _ = new_theory "F4FPPVerifyRefinedMainAtlasEqGoals";

Theorem fast_sound_imp_sound_wrt_domain_atlas_eq:
  !g dom U.
    atlas_eq_equiv /\ sound_wrt_domain g dom U ==> sound_wrt_domain_atlas_eq g dom U
Proof
  rw[atlas_eq_equiv_def, sound_wrt_domain_def, sound_wrt_domain_atlas_eq_def]
  \\ metis_tac[]
QED

Theorem refined_obligations_imply_set_atlas_eq:
  !g dirac.
    (atlas_eq_equiv /\
     slow_refinement_ok g /\
     slow_ok_components_atlas_eq g /\
     fast_semantic_ok g /\
     bottom_layer_total_ok g dirac (U_fast g)) ==>
      (set_atlas_eq (U_fast g) (U_slow g (D_slow g)) /\
       bottom_layer_total_ok g dirac (U_fast g))
Proof
  rpt gen_tac
  \\ strip_tac
  \\ fs[]
  \\ `set_atlas_eq (U_fast g) (U_slow g (D_slow g))` by (
    match_mp_tac sound_and_complete_atlas_eq_gives_set_atlas_eq
    \\ conj_tac >- fs[]
    \\ conj_tac
    >- (
      (* Fast side: existing semantic bundle implies equality-based soundness,
         which implies modulo soundness under reflexivity. *)
      match_mp_tac fast_sound_imp_sound_wrt_domain_atlas_eq
      \\ conj_tac >- fs[]
      \\ `fast_sound g` by metis_tac[fast_semantic_ok_imp_fast_sound]
      \\ fs[fast_sound_def]
      )
    \\ metis_tac[slow_refinement_ok_def, slow_ok_components_atlas_eq_imp_complete_rel_atlas_eq]
    )
  \\ `bottom_layer_total_ok g dirac (U_fast g)` by fs[]
  \\ fs[]
QED

Theorem refined_obligations_imply_set_atlas_eq_fast_atlas_eq:
  !g dirac.
    (atlas_eq_equiv /\
     slow_refinement_ok g /\
     slow_ok_components_atlas_eq g /\
     fast_semantic_ok_atlas_eq g /\
     bottom_layer_total_ok g dirac (U_fast g)) ==>
      (set_atlas_eq (U_fast g) (U_slow g (D_slow g)) /\
       bottom_layer_total_ok g dirac (U_fast g))
Proof
  rpt gen_tac
  \\ strip_tac
  \\ fs[]
  \\ `set_atlas_eq (U_fast g) (U_slow g (D_slow g))` by (
    match_mp_tac sound_and_complete_atlas_eq_gives_set_atlas_eq
    \\ conj_tac >- fs[]
    \\ conj_tac >- metis_tac[fast_semantic_ok_atlas_eq_imp_sound_wrt_domain_atlas_eq]
    \\ metis_tac[slow_refinement_ok_def, slow_ok_components_atlas_eq_imp_complete_rel_atlas_eq]
    )
  \\ `bottom_layer_total_ok g dirac (U_fast g)` by fs[]
  \\ fs[]
QED

(* Variant with the bottom-layer postcondition stated modulo `atlas_eq`. *)
Theorem refined_obligations_imply_set_atlas_eq_and_bottom_layer_total_ok_atlas_eq:
  !g dirac.
    (atlas_eq_equiv /\
     slow_refinement_ok g /\
     slow_ok_components_atlas_eq g /\
     fast_semantic_ok g /\
     bottom_layer_total_ok_atlas_eq g dirac (U_fast g)) ==>
      (set_atlas_eq (U_fast g) (U_slow g (D_slow g)) /\
       bottom_layer_total_ok_atlas_eq g dirac (U_fast g))
Proof
  rpt gen_tac
  \\ strip_tac
  \\ fs[]
  \\ `set_atlas_eq (U_fast g) (U_slow g (D_slow g))` by (
    match_mp_tac sound_and_complete_atlas_eq_gives_set_atlas_eq
    \\ conj_tac >- fs[]
    \\ conj_tac
    >- (
      match_mp_tac fast_sound_imp_sound_wrt_domain_atlas_eq
      \\ conj_tac >- fs[]
      \\ `fast_sound g` by metis_tac[fast_semantic_ok_imp_fast_sound]
      \\ fs[fast_sound_def]
      )
    \\ metis_tac[slow_refinement_ok_def, slow_ok_components_atlas_eq_imp_complete_rel_atlas_eq]
    )
  \\ fs[]
QED

Theorem refined_obligations_imply_set_atlas_eq_fast_atlas_eq_and_bottom_layer_total_ok_atlas_eq:
  !g dirac.
    (atlas_eq_equiv /\
     slow_refinement_ok g /\
     slow_ok_components_atlas_eq g /\
     fast_semantic_ok_atlas_eq g /\
     bottom_layer_total_ok_atlas_eq g dirac (U_fast g)) ==>
      (set_atlas_eq (U_fast g) (U_slow g (D_slow g)) /\
       bottom_layer_total_ok_atlas_eq g dirac (U_fast g))
Proof
  rpt gen_tac
  \\ strip_tac
  \\ fs[]
  \\ `set_atlas_eq (U_fast g) (U_slow g (D_slow g))` by (
    match_mp_tac sound_and_complete_atlas_eq_gives_set_atlas_eq
    \\ conj_tac >- fs[]
    \\ conj_tac >- metis_tac[fast_semantic_ok_atlas_eq_imp_sound_wrt_domain_atlas_eq]
    \\ metis_tac[slow_refinement_ok_def, slow_ok_components_atlas_eq_imp_complete_rel_atlas_eq]
    )
  \\ fs[]
QED

val _ = export_theory ();
