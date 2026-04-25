(*
  File: formal/hol4/F4FPPVerifySlowBridgeDetailedGoalsScript.sml

  Purpose
  - Provide a more detailed bridge layer for the slow checker
    `atlas-scripts-sml/SimplerVerifyF4FPP.sml`.

  Background
  - The slow checker does not (necessarily) materialize a list `dom_list g`;
    it iterates the domain in nested loops:
        x in KGB
        lambda in FPP_lambdas(x)
        gamma in AllBarycenters
    and counts “missing witness” counterexamples relative to the known fast set.
  - In the HOL4 development, we express the same computation in terms of the
    list model `check_domain_fun` over the canonical product list
    `dom_list_from_components g` (see `F4FPPVerifyDomainGoalsTheory` and
    `F4FPPVerifySlowRefineGoalsTheory`).

  This theory isolates the exact bridge obligations we will need to connect
  “slow program succeeds” to those formal predicates:

  - domain enumeration obligations (component list correctness, and agreement
    with the canonical product enumeration), packaged as `slow_refinement_ok g`.
  - “0 misses” obligation stated directly as `slow_ok_components g`.

  Status
  - This theory is “OK” composition-only.
  - The remaining bridge lemma connecting `slow_program_succeeds` to
    `slow_refinement_ok` is isolated in
    `F4FPPVerifySlowBridgeDetailedCheatsGoalsTheory` (currently `cheat`ed).
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySMLBridgeGoalsTheory;
open F4FPPVerifyRefinedMainGoalsTheory;
open F4FPPVerifySlowRefineGoalsTheory;
open F4FPPVerifySlowRefineAtlasEqGoalsTheory;
open F4FPPVerifySlowProgramDecomposeBridgeGoalsTheory;
open F4FPPVerifySlowProgramDecomposeBridgeCheatsGoalsTheory;
open F4FPPVerifySlowRefinementBridgeDecomposeGoalsTheory;
open F4FPPVerifySlowBridgeDetailedCheatsGoalsTheory;

val _ = new_theory "F4FPPVerifySlowBridgeDetailedGoals";

(* Miss-counter bridge: success of the slow program implies “0 misses” in the
   formal list model, i.e. `slow_ok_components`. *)
Theorem slow_program_succeeds_imp_slow_ok_components:
  !g. slow_program_succeeds g ==> slow_ok_components g
Proof
  (* Now available as a derived (still cheat-tainted) lemma from the more
     finely split obligations in
     `F4FPPVerifySlowProgramDecomposeBridgeGoalsTheory`. *)
  metis_tac[slow_program_succeeds_imp_slow_ok_components_decomposed]
QED

Theorem slow_program_succeeds_imp_slow_ok_components_atlas_eq:
  !g. slow_program_succeeds g ==> slow_ok_components_atlas_eq g
Proof
  metis_tac[slow_program_succeeds_imp_slow_ok_components_atlas_eq_decomposed]
QED

(* Convenience: the refined slow obligations used by
   `F4FPPVerifyRefinedBridgeGoalsTheory`. *)
Theorem slow_program_succeeds_imp_refined_slow_obligations_detailed:
  !g. slow_program_succeeds g ==> slow_refinement_ok g /\ slow_ok_components g
Proof
  metis_tac
    [ slow_program_succeeds_imp_slow_refinement_ok
    , slow_program_succeeds_imp_slow_ok_components
    ]
QED

Theorem slow_program_succeeds_imp_refined_slow_obligations_detailed_atlas_eq:
  !g. slow_program_succeeds g ==> slow_refinement_ok g /\ slow_ok_components_atlas_eq g
Proof
  metis_tac
    [ slow_program_succeeds_imp_slow_refinement_ok
    , slow_program_succeeds_imp_slow_ok_components_atlas_eq
    ]
QED

val _ = export_theory ();
