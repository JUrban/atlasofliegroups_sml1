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
  - The bridge theorems are currently `cheat`ed; the goal is to have the
    *right statements* early.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySMLBridgeGoalsTheory;
open F4FPPVerifyRefinedMainGoalsTheory;
open F4FPPVerifySlowRefineGoalsTheory;

val _ = new_theory "F4FPPVerifySlowBridgeDetailedGoals";

(* Domain enumeration bridge: success of the slow program implies the domain
   refinement bundle `slow_refinement_ok`. *)
Theorem slow_program_succeeds_imp_slow_refinement_ok:
  !g. slow_program_succeeds g ==> slow_refinement_ok g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - show `loadBarycenters` enumerates `AllBarycenters g` correctly
    - show `loadLambdasByX` enumerates `FPP_lambdas g x` correctly for each x
    - show the nested `for_domain` loop corresponds to `dom_list_from_components`
      (hence `dom_list_is_components` with an appropriate choice of `dom_list`)
  *)
  cheat
QED

(* Miss-counter bridge: success of the slow program implies “0 misses” in the
   formal list model, i.e. `slow_ok_components`. *)
Theorem slow_program_succeeds_imp_slow_ok_components:
  !g. slow_program_succeeds g ==> slow_ok_components g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - relate `SimplerVerifyF4FPP.triple_is_missing` to `missing_witness`
    - relate the loop counter to `check_domain_fun`
  *)
  cheat
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

val _ = export_theory ();

