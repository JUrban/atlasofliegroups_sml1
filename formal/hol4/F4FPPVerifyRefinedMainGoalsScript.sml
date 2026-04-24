(*
  File: formal/hol4/F4FPPVerifyRefinedMainGoalsScript.sml

  Purpose
  - Provide a *more refined* top-level theorem than `F4FPPVerifyGoalsTheory`,
    by expressing the main `U_slow = U_fast` consequence in terms of smaller
    obligations that correspond more directly to concrete SML modules.

  In particular, we factor:

    Slow side
    - component enumerator correctness (`KGB_list_correct`, etc.)
    - “dom_list is the component product list” (`dom_list_is_components`)
    - slow success predicate (`slow_ok_components`)

    Fast side
    - witness property for `U_fast` over a fast enumeration domain
      (`fast_witnessed`)
    - fast domain inclusion into the intended slow domain (`fast_domain_subset`)
    - bottom-layer checks on `U_fast` (`bottom_layer_total_ok`)

  All lemmas in this file are “OK”: they are purely logical compositions of
  earlier theorems/definitions. The nontrivial work remains in discharging the
  *premises* by connecting them to the concrete Poly/ML programs and Atlas FFI.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyDomainGoalsTheory;
open F4FPPVerifyFastRefineGoalsTheory;
open F4FPPVerifyComponentsBridgeGoalsTheory;
open F4FPPBottomLayerGoalsTheory;

val _ = new_theory "F4FPPVerifyRefinedMainGoals";

(* Bundle the slow-side refinement obligations into a single predicate. *)
Definition slow_refinement_ok_def:
  slow_refinement_ok g <=>
    dom_list_is_components g /\
    KGB_list_correct g /\
    FPP_lambdas_list_correct g /\
    AllBarycenters_list_correct g
End

(* Bundle the fast-side semantic obligations into a single predicate. *)
Definition fast_semantic_ok_def:
  fast_semantic_ok g <=>
    fast_witnessed g /\ fast_domain_subset g
End

Theorem slow_refinement_ok_imp_dom_list_correct:
  !g. slow_refinement_ok g ==> dom_list_correct g
Proof
  rw[slow_refinement_ok_def]
  \\ metis_tac[dom_list_is_components_imp_dom_list_correct]
QED

Theorem slow_refinement_ok_imp_slow_ok_iff_components:
  !g. slow_refinement_ok g ==> (slow_ok g <=> slow_ok_components g)
Proof
  rw[slow_refinement_ok_def]
  \\ metis_tac[dom_list_is_components_imp_slow_ok_iff]
QED

Theorem fast_semantic_ok_imp_fast_sound:
  !g. fast_semantic_ok g ==> fast_sound g
Proof
  rw[fast_semantic_ok_def]
  \\ metis_tac[fast_witnessed_and_subset_imp_fast_sound]
QED

(* Refined main statement: smaller obligations imply the core equivalence
   consequence, plus the bottom-layer invariant package. *)
Theorem refined_obligations_imply_equivalence:
  !g dirac.
    slow_refinement_ok g /\
    slow_ok_components g /\
    fast_semantic_ok g /\
    bottom_layer_total_ok g dirac (U_fast g) ==>
      U_slow g (D_slow g) = U_fast g /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  rpt gen_tac
  \\ rpt disch_tac
  \\ conj_tac
  >- (
    (* Reduce to the earlier top-level equality lemma, supplying its premises. *)
    `dom_list_correct g` by metis_tac[slow_refinement_ok_imp_dom_list_correct]
    \\ `fast_sound g` by metis_tac[fast_semantic_ok_imp_fast_sound]
    \\ `slow_ok g` by metis_tac[slow_refinement_ok_imp_slow_ok_iff_components]
    \\ metis_tac[slow_ok_and_fast_sound_gives_set_equality] )
  \\ simp[]
QED

val _ = export_theory ();
