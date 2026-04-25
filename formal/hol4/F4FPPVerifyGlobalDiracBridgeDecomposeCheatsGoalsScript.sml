(*
  File: formal/hol4/F4FPPVerifyGlobalDiracBridgeDecomposeCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bottom-layer bridge theorems that connect
    `bottom_layer_program_succeeds g dirac` to the per-check obligations and
    their recombination into `bottom_layer_ok_param_set` / `bottom_layer_total_ok`.

  Rationale
  - `F4FPPVerifyGlobalDiracBridgeDecomposeGoalsTheory` is intended to be an
    “OK” decomposition: definitions of per-check obligations and the pure
    recombination lemma showing those obligations imply
    `bottom_layer_ok_param_set`.
  - The actual “execution success ⇒ obligations” statements are program/FFI
    obligations; we keep them in this separate `*Cheats*` theory to avoid
    unnecessary CHEAT-taint propagation.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPBottomLayerParamSetGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;
open F4FPPVerifyGlobalDiracBridgeGoalsTheory;
open F4FPPVerifyGlobalDiracBridgeDecomposeGoalsTheory;
open F4FPPBottomLayerGoalsTheory;
open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPBottomLayerGoalsAtlasEqTheory;
open F4FPPBottomLayerParamSetAtlasEqGoalsTheory;
open F4FPPVerifyFastParamSetAtlasEqGoalsTheory;

val _ = new_theory "F4FPPVerifyGlobalDiracBridgeDecomposeCheatsGoals";

(* --- Per-check bridge lemmas: success implies each check obligation (CHEATED). --- *)

Theorem bottom_layer_program_succeeds_imp_bl_standard_final_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_standard_final_ok g
Proof
  cheat
QED

Theorem bottom_layer_program_succeeds_imp_bl_lambda_table_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_lambda_table_ok g
Proof
  cheat
QED

Theorem bottom_layer_program_succeeds_imp_bl_twist_equiv_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_twist_equiv_ok g
Proof
  cheat
QED

Theorem bottom_layer_program_succeeds_imp_bl_hermitian_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_hermitian_ok g
Proof
  cheat
QED

Theorem bottom_layer_program_succeeds_imp_bl_unitary_if_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_unitary_if_ok g dirac
Proof
  cheat
QED

Theorem bottom_layer_program_succeeds_imp_bl_dual_closed_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_dual_closed_ok g
Proof
  cheat
QED

Theorem bottom_layer_program_succeeds_imp_bl_rho_seeded_ok:
  !g dirac. bottom_layer_program_succeeds g dirac ==> bl_rho_seeded_ok g
Proof
  cheat
QED

(* --- Derived bridge: success implies the full `bottom_layer_ok_param_set` (OK composition). --- *)

Theorem bottom_layer_program_succeeds_imp_bottom_layer_ok_param_set_decomposed:
  !g dirac.
    bottom_layer_program_succeeds g dirac ==>
      bottom_layer_ok_param_set g dirac (fast_param_set g)
Proof
  rpt strip_tac
  \\ match_mp_tac bl_checks_imp_bottom_layer_ok_param_set
  \\ metis_tac
       [ bottom_layer_program_succeeds_imp_bl_standard_final_ok
       , bottom_layer_program_succeeds_imp_bl_lambda_table_ok
       , bottom_layer_program_succeeds_imp_bl_twist_equiv_ok
       , bottom_layer_program_succeeds_imp_bl_hermitian_ok
       , bottom_layer_program_succeeds_imp_bl_unitary_if_ok
       , bottom_layer_program_succeeds_imp_bl_dual_closed_ok
       ]
QED

(* Downstream consequences (OK composition, but cheat-tainted by the bridges above). *)

Theorem bottom_layer_program_succeeds_and_fast_param_set_ok_imp_bottom_layer_ok_decomposed:
  !g dirac.
    bottom_layer_program_succeeds g dirac /\ fast_param_set_ok g ==>
      bottom_layer_ok g dirac (U_fast g)
Proof
  rpt strip_tac
  \\ match_mp_tac fast_param_set_ok_and_bottom_layer_ok_param_set_imp_bottom_layer_ok
  \\ conj_tac
  >- simp[]
  \\ metis_tac[bottom_layer_program_succeeds_imp_bottom_layer_ok_param_set_decomposed]
QED

Theorem bottom_layer_program_succeeds_and_fast_param_set_ok_imp_total_ok_noncompact_decomposed:
  !g dirac.
    bottom_layer_program_succeeds g dirac /\ fast_param_set_ok g /\ ~group_is_compact g ==>
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  rw[bottom_layer_total_ok_def]
  \\ metis_tac[bottom_layer_program_succeeds_and_fast_param_set_ok_imp_bottom_layer_ok_decomposed]
QED

(* A more convenient all-groups composition lemma:
   - compact groups: use the rho-seeding postcondition,
   - noncompact: use the bottom-layer check conjunction. *)
Theorem bottom_layer_program_succeeds_and_fast_param_set_ok_imp_total_ok_decomposed:
  !g dirac.
    bottom_layer_program_succeeds g dirac /\ fast_param_set_ok g ==>
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  rpt strip_tac
  \\ Cases_on `group_is_compact g`
  >- (
    rw[bottom_layer_total_ok_def, SUBSET_DEF]
    \\ drule bottom_layer_program_succeeds_imp_bl_rho_seeded_ok
    \\ strip_tac
    \\ fs[bl_rho_seeded_ok_def, fast_param_set_ok_def, param_set_rep_ok_def]
    \\ metis_tac[] )
  \\ rw[bottom_layer_total_ok_def]
  \\ metis_tac[bottom_layer_program_succeeds_and_fast_param_set_ok_imp_bottom_layer_ok_decomposed]
QED

(* --------------------------------------------------------------------- *)
(*  Modulo-`atlas_eq` variants                                            *)
(* --------------------------------------------------------------------- *)

Theorem bottom_layer_program_succeeds_imp_bottom_layer_total_ok_param_set_atlas_eq_decomposed:
  !g dirac.
    bottom_layer_program_succeeds g dirac ==>
      bottom_layer_total_ok_param_set_atlas_eq g dirac (fast_param_set g)
Proof
  rpt strip_tac
  \\ Cases_on `group_is_compact g`
  \\ fs[bottom_layer_total_ok_param_set_atlas_eq_def]
  >- metis_tac[bottom_layer_program_succeeds_imp_bl_rho_seeded_ok, bl_rho_seeded_ok_def]
  \\ metis_tac[bottom_layer_program_succeeds_imp_bottom_layer_ok_param_set_decomposed]
QED

Theorem bottom_layer_program_succeeds_and_fast_param_set_ok_atlas_eq_imp_total_ok_atlas_eq_decomposed:
  !g dirac.
    atlas_eq_equiv /\ atlas_eq_congruent_bottom_layer /\
    bottom_layer_program_succeeds g dirac /\ fast_param_set_ok_atlas_eq g ==>
      bottom_layer_total_ok_atlas_eq g dirac (U_fast g)
Proof
  rpt strip_tac
  \\ match_mp_tac
       fast_param_set_ok_atlas_eq_and_bottom_layer_total_ok_param_set_atlas_eq_imp_bottom_layer_total_ok_atlas_eq
  \\ asm_rewrite_tac[]
  \\ metis_tac[bottom_layer_program_succeeds_imp_bottom_layer_total_ok_param_set_atlas_eq_decomposed]
QED

val _ = export_theory ();
