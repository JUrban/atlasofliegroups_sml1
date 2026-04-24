(*
  File: formal/hol4/F4FPPBottomLayerGoalsScript.sml

  Purpose
  - Top-down *specification layer* for the “bottom-layer” consistency checks
    implemented in `atlas-scripts-sml/FPP_globalDirac.sml`.
  - These checks are not the core “fast vs slow completeness” argument, but
    they are part of the full functionality of `VerifyF4FPP.sml` and should be
    captured as explicit predicates/theorems:
      - standard/final sanity
      - (F4-specific) lambda-table consistency
      - twist-equivalence
      - hermitian
      - optionally unitary (guarded by the Dirac flag)
      - closure under contragredient (“unitary dual symmetry”)
      - compact-group special-casing (rho seeding instead of checks)

  Scope / status
  - This theory introduces *abstract primitives* (uninterpreted functions and
    predicates) that will later be refined to Atlas FFI semantics.
  - The definitions here are stable and intended to match the SML code at the
    level of “what property is being checked”.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;

val _ = new_theory "F4FPPBottomLayerGoals";

(* --- Abstract semantic primitives to be refined later --- *)

val _ = new_constant ("is_standard", ``:param -> bool``);
val _ = new_constant ("is_final", ``:param -> bool``);
val _ = new_constant ("is_hermitian", ``:param -> bool``);

val _ = new_constant ("twist", ``:param -> param``);
val _ = new_constant ("param_equiv", ``:param -> param -> bool``);

val _ = new_constant ("contragredient", ``:param -> param``);

(* Mirrors the SML behaviour:
   - the lambda-table check is only enabled for certain groups (in the current
     code, exactly when `kgbSize = 229`, i.e. the split `F4` case),
   - and when enabled, each param must satisfy a group-specific predicate. *)
val _ = new_constant ("needs_lambda_table_check", ``:group -> bool``);
val _ = new_constant ("lambda_table_ok", ``:group -> param -> bool``);

(* Mirrors the SML special-casing for compact groups: instead of running the
   bottom-layer checks, the implementation seeds the set with the parameters at
   infinitesimal character `rho`. *)
val _ = new_constant ("group_is_compact", ``:group -> bool``);
val _ = new_constant ("rho_set", ``:group -> param set``);

(* --- Set-level specifications of each check --- *)

Definition standard_final_ok_def:
  standard_final_ok (U:param set) <=>
    !p. p IN U ==> is_standard p /\ is_final p
End

Definition lambda_table_set_ok_def:
  lambda_table_set_ok g (U:param set) <=>
    needs_lambda_table_check g ==> (!p. p IN U ==> lambda_table_ok g p)
End

Definition twist_equiv_ok_def:
  twist_equiv_ok (U:param set) <=>
    !p. p IN U ==> param_equiv p (twist p)
End

Definition hermitian_ok_def:
  hermitian_ok (U:param set) <=>
    !p. p IN U ==> is_hermitian p
End

Definition unitary_ok_def:
  unitary_ok (U:param set) <=>
    !p. p IN U ==> is_unitary p
End

Definition unitary_if_def:
  unitary_if (dirac:bool) (U:param set) <=>
    if dirac then unitary_ok U else T
End

Definition dual_closed_def:
  dual_closed (U:param set) <=>
    !p. p IN U ==> contragredient p IN U
End

(* The full conjunction corresponding to `FPP_unitary_hash_bottom_layer_set`. *)
Definition bottom_layer_ok_def:
  bottom_layer_ok g (dirac:bool) (U:param set) <=>
    standard_final_ok U /\
    lambda_table_set_ok g U /\
    twist_equiv_ok U /\
    hermitian_ok U /\
    unitary_if dirac U /\
    dual_closed U
End

(* Total spec corresponding to `FPP_unitary_hash_bottom_layer_param_hash`:
   - compact groups: require the rho-seeding postcondition
   - non-compact: require the bottom-layer check conjunction. *)
Definition bottom_layer_total_ok_def:
  bottom_layer_total_ok g (dirac:bool) (U:param set) <=>
    if group_is_compact g then
      rho_set g SUBSET U
    else
      bottom_layer_ok g dirac U
End

(* Small derived lemma used when splitting goals. *)
Theorem bottom_layer_ok_imp_dual_closed:
  !g dirac U. bottom_layer_ok g dirac U ==> dual_closed U
Proof
  simp[bottom_layer_ok_def]
QED

val _ = export_theory ();

