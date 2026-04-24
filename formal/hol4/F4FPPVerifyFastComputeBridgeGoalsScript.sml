(*
  File: formal/hol4/F4FPPVerifyFastComputeBridgeGoalsScript.sml

  Purpose
  - Split the fast program’s bridge obligations into a piece corresponding to
    the *construction* of the fast set (via `F4_FPP_points_compute` and
    `ParamHash`) and a piece corresponding to the *verification* of that set
    (handled separately in `F4FPPVerifyGlobalDiracBridgeGoalsTheory`).

  Concrete SML this targets
  - `atlas-scripts-sml/F4_FPP_points_compute.sml`
  - `atlas-scripts-sml/ParamHash.sml`
  - invoked from `atlas-scripts-sml/VerifyF4FPP.sml`

  Obligations this theory isolates
  - “What domain is considered?”:
      `fast_domain_is_pruned g` (i.e. `D_fast g` is the pruned slow domain)
  - “Every stored element is justified by a witness triple”:
      `fast_witnessed_pruned g`
  - “The param_set interface is faithful”:
      `fast_param_set_ok g`

  From these we can derive (OK, without cheating):
  - `fast_semantic_ok g` (the bundle used by the refined main theorem)

  Status
  - The bridge theorem linking “fast compute program succeeds” to the
    obligations is currently `cheat`ed. The rest is OK composition.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyFastPruneGoalsTheory;
open F4FPPVerifyFastRefineGoalsTheory;
open F4FPPVerifyRefinedMainGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;
open F4FPPVerifyFastParamSetListRefineGoalsTheory;
open F4FPPVerifyFastParamSetContainsRefineGoalsTheory;

val _ = new_theory "F4FPPVerifyFastComputeBridgeGoals";

(* Abstract predicate: the fast compute phase (building the ParamHash)
   returned successfully (no exception raised). *)
val _ = new_constant ("fast_compute_program_succeeds", ``:group -> bool``);

(* Bundle the compute-phase obligations in one predicate, to simplify bridge
   statements. *)
Definition fast_compute_obligations_def:
  fast_compute_obligations g <=>
    fast_domain_is_pruned g /\
    fast_witnessed_pruned g /\
    fast_param_set_list_sound g /\
    fast_param_set_list_complete g /\
    fast_param_set_contains_sound g /\
    fast_param_set_contains_complete g
End

(* The actual bridge: success implies the obligations (CHEATED for now). *)
Theorem fast_compute_program_succeeds_imp_obligations:
  !g. fast_compute_program_succeeds g ==> fast_compute_obligations g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - a domain accounting argument for which triples are enumerated by the
      implementation (`fast_considers`),
    - a witness argument tying every stored parameter to some considered triple,
    - a data-structure argument that `ParamHash.list/contains` satisfy
      `fast_param_set_ok`.
  *)
  cheat
QED

(* OK: compute obligations imply the semantic obligations used by the refined
   main theorem. *)
Theorem fast_compute_obligations_imp_fast_semantic_ok:
  !g. fast_compute_obligations g ==> fast_semantic_ok g
Proof
  rw[fast_compute_obligations_def, fast_semantic_ok_def]
  \\ drule fast_pruned_obligations_imp_fast_semantic_obligations
  \\ metis_tac[]
QED

Theorem fast_compute_obligations_imp_fast_param_set_ok:
  !g. fast_compute_obligations g ==> fast_param_set_ok g
Proof
  rw[fast_compute_obligations_def]
  \\ metis_tac[fast_param_set_list_and_contains_obligations_imp_fast_param_set_ok]
QED

val _ = export_theory ();
