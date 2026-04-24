(*
  File: formal/hol4/F4FPPVerifyFastComputeBridgeDecomposeGoalsScript.sml

  Purpose
  - Further decompose the (currently cheated) bridge
      `fast_compute_program_succeeds g ⇒ fast_compute_obligations g`
    from `F4FPPVerifyFastComputeBridgeGoalsTheory` into two sharper bundles:

      (A) domain/witness accounting obligations (about which triples are
          considered and how they witness stored parameters), and
      (B) ParamHash interface obligations (list/contains sound+complete),
          expressed via `paramhash_ok` from `F4FPPVerifyParamHashBridgeGoalsTheory`.

  Motivation
  - This isolates the two main “hard parts” of the compute phase:
      - proving the computed set is semantically justified (witnessed), and
      - proving the imperative hash structure faithfully implements the
        list/contains interface used by the bottom-layer checker.
  - Everything else is pure logical composition and should remain “OK”.

  Status
  - The bridge from `fast_compute_program_succeeds` to the two bundles is
    stated here; the ParamHash part can re-use the already-stated cheated lemma
    `fast_compute_program_succeeds_imp_paramhash_ok`.
  - The recombination lemmas are all OK.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyFastPruneGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;
open F4FPPVerifyFastParamSetListRefineGoalsTheory;
open F4FPPVerifyFastParamSetContainsRefineGoalsTheory;

val _ = new_theory "F4FPPVerifyFastComputeBridgeDecomposeGoals";

(* Bundle (A): domain/witness accounting (purely semantic). *)
Definition fast_compute_domain_ok_def:
  fast_compute_domain_ok g <=>
    fast_domain_is_pruned g /\
    fast_witnessed_pruned g
End

(* Bundle (B): data-structure interface correctness, via ParamHash. *)
Definition fast_compute_paramhash_ok_def:
  fast_compute_paramhash_ok g <=>
    paramhash_obligations_factored g
End

Theorem fast_compute_domain_and_paramhash_ok_imp_fast_compute_obligations:
  !g.
    fast_compute_domain_ok g /\ fast_compute_paramhash_ok g ==>
      fast_compute_obligations g
Proof
  rpt strip_tac
  \\ `paramhash_ok g` by
       (fs[fast_compute_paramhash_ok_def]
        \\ metis_tac[paramhash_obligations_factored_imp_paramhash_ok])
  \\ fs[paramhash_ok_def]
  \\ rw[fast_compute_obligations_def]
  >- fs[fast_compute_domain_ok_def]
  >- fs[fast_compute_domain_ok_def]
  >- metis_tac[fast_param_set_is_paramhash_and_paramhash_list_sound_imp_list_sound]
  >- metis_tac[fast_param_set_is_paramhash_and_paramhash_list_complete_imp_list_complete]
  >- metis_tac[fast_param_set_is_paramhash_and_paramhash_contains_sound_imp_contains_sound]
  \\ metis_tac[fast_param_set_is_paramhash_and_paramhash_contains_complete_imp_contains_complete]
QED

(* --- Bridge obligations from compute-phase success (currently CHEATED) --- *)

Theorem fast_compute_program_succeeds_imp_fast_compute_domain_ok:
  !g. fast_compute_program_succeeds g ==> fast_compute_domain_ok g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - identify the exact pruning predicate (`fast_considers`) implemented by
      `F4_FPP_points_compute` (bucket/key matching + additional filters),
    - show `D_fast g = { t ∈ D_slow g | fast_considers g t }`,
    - show every stored element of `U_fast g` is witnessed by such a triple.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_fast_compute_paramhash_ok:
  !g. fast_compute_program_succeeds g ==> fast_compute_paramhash_ok g
Proof
  rw[fast_compute_paramhash_ok_def]
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_obligations_factored]
QED

(* Derived (cheat-tainted) bridge: the compute phase implies the original
   `fast_compute_obligations`. This reduces the remaining proof work to the two
   explicit bundles above. *)
Theorem fast_compute_program_succeeds_imp_fast_compute_obligations_factored:
  !g. fast_compute_program_succeeds g ==> fast_compute_obligations g
Proof
  rpt strip_tac
  \\ match_mp_tac fast_compute_domain_and_paramhash_ok_imp_fast_compute_obligations
  \\ conj_tac
  >- metis_tac[fast_compute_program_succeeds_imp_fast_compute_domain_ok]
  \\ metis_tac[fast_compute_program_succeeds_imp_fast_compute_paramhash_ok]
QED

val _ = export_theory ();
