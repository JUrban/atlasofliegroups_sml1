(*
  File: formal/hol4/F4FPPVerifyRefinedBridgeGoalsScript.sml

  Purpose
  - Provide a *more detailed* “SML bridge” layer than
    `F4FPPVerifySMLBridgeGoalsTheory`.
  - Instead of stating that fast/slow program success implies the fully
    packaged predicate `full_ok`, we connect success to the *refined*
    obligation bundles used by `F4FPPVerifyRefinedMainGoalsTheory`:

      Slow side (from `SimplerVerifyF4FPP.sml`)
      - `slow_refinement_ok g`  (domain enumeration matches component product)
      - `slow_ok_components g`  (0 misses over the component product list)

      Fast side (from `VerifyF4FPP.sml` and its dependencies)
      - `fast_semantic_ok g`    (witnessed + domain-subset obligations)
      - `bottom_layer_total_ok g dirac (U_fast g)` (post-check invariants)

  Status
  - The bridge theorems here are intentionally **top-down** and therefore
    use `cheat` for now; they are a place to attach the eventual CakeML proofs
    and Atlas/C++ FFI specifications.
  - The final theorem in this file is an “OK” logical composition *given* the
    bridge obligations.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifySlowRefineGoalsTheory;
open F4FPPVerifySlowRefineAtlasEqGoalsTheory;
open F4FPPVerifyFastRefineGoalsTheory;
open F4FPPVerifyFastRefineAtlasEqGoalsTheory;
open F4FPPBottomLayerGoalsTheory;
open F4FPPVerifyRefinedMainGoalsTheory;
open F4FPPVerifyRefinedMainAtlasEqGoalsTheory;
	open F4FPPVerifyAtlasFFIContractsGoalsTheory;
	open F4FPPVerifyAtlasEqSetGoalsTheory;
	open F4FPPBottomLayerGoalsAtlasEqTheory;
	open F4FPPVerifySMLBridgeGoalsTheory;
	open F4FPPVerifySlowBridgeDetailedGoalsTheory;

val _ = new_theory "F4FPPVerifyRefinedBridgeGoals";

(* --- Refined bridge obligations (currently CHEATED) --- *)

Theorem fast_program_succeeds_imp_refined_fast_obligations:
  !g dirac.
    fast_program_succeeds g dirac ==>
      fast_semantic_ok g /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - a proof/spec that `F4_FPP_points_compute.computeAllIntoParamHash`
      constructs a set `U_fast g` satisfying `fast_witnessed g` and
      `fast_domain_subset g` (hence `fast_semantic_ok g`),
    - a proof/spec that `FPP_globalDirac.FPP_unitary_hash_bottom_layer_param_hash`
      enforces `bottom_layer_total_ok g dirac (U_fast g)` for the resulting set.
  *)
  cheat
QED

Theorem fast_program_succeeds_imp_refined_fast_obligations_atlas_eq:
  !g dirac.
    fast_program_succeeds g dirac ==>
      fast_semantic_ok_atlas_eq g /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - as for `fast_program_succeeds_imp_refined_fast_obligations`, but with the
      weaker witness property `fast_witnessed_atlas_eq`, allowing the fast set
      to contain different representatives of the same semantic Atlas param.
  *)
  cheat
QED

Theorem slow_program_succeeds_imp_refined_slow_obligations:
  !g.
    slow_program_succeeds g ==>
      slow_refinement_ok g /\
      slow_ok_components g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - a proof/spec that the `SimplerVerifyF4FPP` domain iteration matches the
      component-product enumeration (`dom_list_is_components` + component-list
      correctness),
    - a proof/spec that the counterexample counter computed by the slow program
      is exactly `check_domain_fun` over the same enumeration, so “0 misses” is
      `slow_ok_components g`.
  *)
  cheat
QED

Theorem slow_program_succeeds_imp_refined_slow_obligations_atlas_eq:
  !g.
    slow_program_succeeds g ==>
      slow_refinement_ok g /\
      slow_ok_components_atlas_eq g
Proof
  metis_tac[slow_program_succeeds_imp_refined_slow_obligations_detailed_atlas_eq]
QED

(* --- Derived end-user theorem (tainted by the cheated bridge obligations) --- *)

Theorem fast_and_slow_programs_succeed_gives_refined_equivalence:
  !g dirac.
    fast_program_succeeds g dirac /\ slow_program_succeeds g ==>
      U_slow g (D_slow g) = U_fast g /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  rpt strip_tac
  \\ mp_tac (SPEC_ALL fast_program_succeeds_imp_refined_fast_obligations)
  \\ mp_tac (SPEC_ALL slow_program_succeeds_imp_refined_slow_obligations)
  \\ metis_tac[refined_obligations_imply_equivalence]
QED

Theorem fast_and_slow_programs_succeed_gives_refined_equivalence_atlas_eq:
  !g dirac.
    atlas_hash_eq_ok /\
    fast_program_succeeds g dirac /\ slow_program_succeeds g ==>
      set_atlas_eq (U_fast g) (U_slow g (D_slow g)) /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  rpt strip_tac
  \\ mp_tac (SPEC_ALL fast_program_succeeds_imp_refined_fast_obligations)
  \\ mp_tac (SPEC_ALL slow_program_succeeds_imp_refined_slow_obligations_atlas_eq)
  \\ `atlas_eq_equiv` by fs[atlas_hash_eq_ok_def]
  \\ metis_tac[refined_obligations_imply_set_atlas_eq]
QED

Theorem fast_and_slow_programs_succeed_gives_refined_equivalence_atlas_eq_fast_atlas_eq:
  !g dirac.
    atlas_hash_eq_ok /\
    fast_program_succeeds g dirac /\ slow_program_succeeds g ==>
      set_atlas_eq (U_fast g) (U_slow g (D_slow g)) /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  rpt strip_tac
  \\ mp_tac (SPEC_ALL fast_program_succeeds_imp_refined_fast_obligations_atlas_eq)
  \\ mp_tac (SPEC_ALL slow_program_succeeds_imp_refined_slow_obligations_atlas_eq)
  \\ `atlas_eq_equiv` by fs[atlas_hash_eq_ok_def]
  \\ metis_tac[refined_obligations_imply_set_atlas_eq_fast_atlas_eq]
QED

(* Derived variant: also expose the bottom-layer postcondition modulo `atlas_eq`. *)
Theorem fast_and_slow_programs_succeed_gives_refined_equivalence_atlas_eq_bottom_layer_atlas_eq:
  !g dirac.
    atlas_hash_eq_ok /\ atlas_eq_congruent_bottom_layer /\
    fast_program_succeeds g dirac /\ slow_program_succeeds g ==>
      set_atlas_eq (U_fast g) (U_slow g (D_slow g)) /\
      bottom_layer_total_ok_atlas_eq g dirac (U_fast g)
Proof
  rpt strip_tac
  \\ `set_atlas_eq (U_fast g) (U_slow g (D_slow g)) /\
      bottom_layer_total_ok g dirac (U_fast g)` by
       metis_tac[fast_and_slow_programs_succeed_gives_refined_equivalence_atlas_eq]
  \\ `atlas_eq_equiv` by fs[atlas_hash_eq_ok_def]
  \\ metis_tac[bottom_layer_total_ok_imp_bottom_layer_total_ok_atlas_eq]
QED

Theorem fast_and_slow_programs_succeed_gives_refined_equivalence_atlas_eq_fast_atlas_eq_bottom_layer_atlas_eq:
  !g dirac.
    atlas_hash_eq_ok /\ atlas_eq_congruent_bottom_layer /\
    fast_program_succeeds g dirac /\ slow_program_succeeds g ==>
      set_atlas_eq (U_fast g) (U_slow g (D_slow g)) /\
      bottom_layer_total_ok_atlas_eq g dirac (U_fast g)
Proof
  rpt strip_tac
  \\ `set_atlas_eq (U_fast g) (U_slow g (D_slow g)) /\
      bottom_layer_total_ok g dirac (U_fast g)` by
       metis_tac[fast_and_slow_programs_succeed_gives_refined_equivalence_atlas_eq_fast_atlas_eq]
  \\ `atlas_eq_equiv` by fs[atlas_hash_eq_ok_def]
  \\ metis_tac[bottom_layer_total_ok_imp_bottom_layer_total_ok_atlas_eq]
QED

val _ = export_theory ();
