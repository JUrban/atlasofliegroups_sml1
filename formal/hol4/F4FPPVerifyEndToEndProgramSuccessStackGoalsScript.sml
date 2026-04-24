(*
  File: formal/hol4/F4FPPVerifyEndToEndProgramSuccessStackGoalsScript.sml

  Purpose
  - Record a fully explicit “program success ⇒ obligations ⇒ equivalence”
    theorem that composes the existing bridge layers.

  What this gives you
  - A single theorem whose proof is almost entirely *OK composition*:
      it just applies the named bridge lemmas and the obligation-stack theorem
      from `F4FPPVerifyEndToEndObligationStackGoalsTheory`.
  - The only remaining `cheat` surface is where it should be: the bridge
    lemmas that connect concrete Poly/ML + Atlas FFI execution to the abstract
    obligations.

  Status
  - The theorem here is `CHEAT`-tainted because it depends on earlier cheated
    bridge lemmas, but it introduces no new `cheat`s of its own.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifySMLBridgeGoalsTheory;
open F4FPPVerifyFastProgramSplitBridgeGoalsTheory;
open F4FPPVerifyFastComputeBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateDecomposeGoalsTheory;
open F4FPPVerifyGlobalDiracBridgeDecomposeGoalsTheory;
open F4FPPVerifySlowBridgeDetailedGoalsTheory;
open F4FPPVerifyEndToEndObligationStackGoalsTheory;
open F4FPPVerifyTargetGroupGoalsTheory;

val _ = new_theory "F4FPPVerifyEndToEndProgramSuccessStackGoals";

Theorem program_success_implies_equivalence_via_obligation_stack:
  !g dirac.
    atlas_eq_is_hol_eq /\ atlas_hash_range /\ ~group_is_compact g /\
    fast_program_succeeds g dirac /\ slow_program_succeeds g ==>
      U_slow g (D_slow g) = U_fast g /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  rpt gen_tac
  \\ rpt strip_tac

  \\ drule fast_program_succeeds_imp_phase_success
  \\ disch_then strip_assume_tac

  \\ `fast_compute_domain_ok g` by
       metis_tac[fast_compute_program_succeeds_imp_fast_compute_domain_ok]

  \\ `paramhash_obligations_state_factored g` by
       metis_tac[fast_compute_program_succeeds_imp_paramhash_obligations_state_factored]

  \\ `bottom_layer_ok_param_set g dirac (fast_param_set g)` by
       metis_tac[bottom_layer_program_succeeds_imp_bottom_layer_ok_param_set_decomposed]

  \\ `slow_refinement_ok g /\ slow_ok_components g` by
       metis_tac[slow_program_succeeds_imp_refined_slow_obligations_detailed]

  \\ metis_tac[obligations_stack_imply_equivalence]
QED

(* A convenient specialization for the concrete target group `F4s`. *)
Theorem program_success_implies_equivalence_via_obligation_stack_F4s:
  !dirac.
    atlas_eq_is_hol_eq /\ atlas_hash_range /\
    fast_program_succeeds F4s dirac /\ slow_program_succeeds F4s ==>
      U_slow F4s (D_slow F4s) = U_fast F4s /\
      bottom_layer_total_ok F4s dirac (U_fast F4s)
Proof
  rpt gen_tac
  \\ rpt strip_tac
  \\ metis_tac
      [ program_success_implies_equivalence_via_obligation_stack
      , F4s_not_compact
      ]
QED

val _ = export_theory ();
