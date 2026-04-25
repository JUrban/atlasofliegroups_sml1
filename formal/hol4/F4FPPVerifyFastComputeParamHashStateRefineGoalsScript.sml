(*
  File: formal/hol4/F4FPPVerifyFastComputeParamHashStateRefineGoalsScript.sml

  Purpose
  - Provide a refinement-friendly route from fast compute-phase success to the
    ParamHash obligations used by the fast bridge stack, via the *state-level*
    bundle `paramhash_obligations_state_factored`.

  Key point
  - We already have (cheated) statements of the form:
      `fast_compute_program_succeeds g ⇒ paramhash_obligations_factored g`
    and therefore `fast_compute_program_succeeds g ⇒ fast_compute_paramhash_ok g`.
  - This theory records a sharper two-step proof structure:
      (1) `fast_compute_program_succeeds g ⇒ paramhash_obligations_state_factored g`
          (execution ⇒ state witness + wiring + stores-U-fast),
      (2) under explicit Atlas hash/equality contracts,
          `paramhash_obligations_state_factored g ⇒ paramhash_obligations_factored g`.

  This isolates the part intended to be discharged by CakeML/translator proofs
  (state invariants) from the purely logical recombination.

  Status
  - Composition lemmas are OK.
  - The bridge from execution to `paramhash_obligations_state_factored` remains
    `cheat`ed in `F4FPPVerifyParamHashBridgeStateDecomposeGoalsTheory`.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyFastComputeBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateDecomposeCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildDecomposeCheatsGoalsTheory;

val _ = new_theory "F4FPPVerifyFastComputeParamHashStateRefineGoals";

Theorem fast_compute_program_succeeds_imp_fast_compute_paramhash_ok_via_state:
  !g.
    atlas_eq_is_hol_eq /\ atlas_hash_range /\ fast_compute_program_succeeds g ==>
      fast_compute_paramhash_ok g
Proof
  rpt strip_tac
  \\ rw[fast_compute_paramhash_ok_def]
  \\ match_mp_tac paramhash_state_factored_imp_paramhash_obligations_factored
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_obligations_state_factored]
QED

(* More specified route: use the build-witness–factored ParamHash bundle.

   This pushes the “translator/CakeML” boundary towards proving:
     `fast_compute_program_succeeds ==> paramhash_build_witness`
   rather than only an existential `paramhash_state_ok`. *)
Theorem fast_compute_program_succeeds_imp_fast_compute_paramhash_ok_via_build:
  !g.
    atlas_eq_is_hol_eq /\ atlas_hash_range /\ fast_compute_program_succeeds g ==>
      fast_compute_paramhash_ok g
Proof
  rpt strip_tac
  \\ rw[fast_compute_paramhash_ok_def]
  \\ match_mp_tac atlas_hash_range_and_build_factored_imp_paramhash_obligations_factored
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_obligations_build_factored]
QED

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_factored_atlas_eq_via_build:
  !g.
    atlas_hash_eq_ok /\ fast_compute_program_succeeds g ==>
      paramhash_obligations_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ match_mp_tac atlas_hash_eq_ok_and_build_factored_atlas_eq_imp_paramhash_obligations_factored_atlas_eq
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_obligations_build_factored_atlas_eq]
QED

(* Modulo-`atlas_eq` variant: avoid `atlas_eq_is_hol_eq` in the ParamHash layer. *)
Theorem fast_compute_program_succeeds_imp_paramhash_obligations_factored_atlas_eq_via_state:
  !g.
    atlas_hash_eq_ok /\ fast_compute_program_succeeds g ==>
      paramhash_obligations_factored_atlas_eq g
Proof
  rpt strip_tac
  \\ match_mp_tac paramhash_state_factored_atlas_eq_imp_paramhash_obligations_factored_atlas_eq
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_obligations_state_factored_atlas_eq]
QED

val _ = export_theory ();
