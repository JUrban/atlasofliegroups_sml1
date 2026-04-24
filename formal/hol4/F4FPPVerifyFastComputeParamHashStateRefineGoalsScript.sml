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

val _ = export_theory ();

