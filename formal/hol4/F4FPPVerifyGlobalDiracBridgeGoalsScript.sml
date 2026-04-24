(*
  File: formal/hol4/F4FPPVerifyGlobalDiracBridgeGoalsScript.sml

  Purpose
  - Provide a dedicated bridge layer for the `FPP_globalDirac.sml` bottom-layer
    verification pipeline.

  Why a separate bridge?
  - The fast program `VerifyF4FPP.sml` is conceptually two phases:
      (1) build the unitary parameter hash (fast semantics obligations), and
      (2) run the bottom-layer checks / rho-seeding (`FPP_globalDirac`).
  - We already have:
      - a set-level spec (`bottom_layer_total_ok`) in
        `F4FPPBottomLayerGoalsTheory`,
      - a param_set-level spec that matches SML’s interface in
        `F4FPPBottomLayerParamSetGoalsTheory`,
      - and a named param_set object `fast_param_set g` plus its representation
        obligation (`fast_param_set_ok`) in
        `F4FPPVerifyFastParamSetGoalsTheory`.
  - This theory records, top-down, the precise theorem statements we will want
    about `FPP_globalDirac` success.

  Status
  - The bridge theorems are currently `cheat`ed; the “composition” lemmas are
    OK once the bridge obligations hold.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPBottomLayerGoalsTheory;
open F4FPPBottomLayerParamSetGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;

val _ = new_theory "F4FPPVerifyGlobalDiracBridgeGoals";

(* Abstract predicate: the SML bottom-layer pipeline returned successfully for
   the fast program’s param_set (no exception raised). *)
val _ = new_constant ("bottom_layer_program_succeeds", ``:group -> bool -> bool``);

(* Bridge obligation: success implies the param_set-style predicate. *)
Theorem bottom_layer_program_succeeds_imp_bottom_layer_ok_param_set:
  !g dirac.
    bottom_layer_program_succeeds g dirac ==>
      bottom_layer_ok_param_set g dirac (fast_param_set g)
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - relate `ParamHash.list` / `ParamHash.contains` to `fast_param_set g`,
    - show each SML check (filter/contains-based) enforces the corresponding
      conjunct in `bottom_layer_ok_param_set_def`,
    - show the function’s success means all checks passed.
  *)
  cheat
QED

(* Composition: once the param_set representation and bridge obligations hold,
   we get the set-level `bottom_layer_ok` property for `U_fast g`. *)
Theorem bottom_layer_program_succeeds_and_fast_param_set_ok_imp_bottom_layer_ok:
  !g dirac.
    bottom_layer_program_succeeds g dirac /\ fast_param_set_ok g ==>
      bottom_layer_ok g dirac (U_fast g)
Proof
  rw[]
  \\ match_mp_tac fast_param_set_ok_and_bottom_layer_ok_param_set_imp_bottom_layer_ok
  \\ conj_tac
  >- simp[]
  \\ metis_tac[bottom_layer_program_succeeds_imp_bottom_layer_ok_param_set]
QED

(* For non-compact groups, this also yields `bottom_layer_total_ok`. *)
Theorem bottom_layer_program_succeeds_and_fast_param_set_ok_imp_total_ok_noncompact:
  !g dirac.
    bottom_layer_program_succeeds g dirac /\ fast_param_set_ok g /\ ~group_is_compact g ==>
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  rw[bottom_layer_total_ok_def]
  \\ metis_tac[bottom_layer_program_succeeds_and_fast_param_set_ok_imp_bottom_layer_ok]
QED

val _ = export_theory ();
