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
    `fast_compute_program_succeeds_imp_paramhash_obligations_factored` (from the
    isolated `*Cheats*` theory `F4FPPVerifyParamHashBridgeDecomposeCheatsGoalsTheory`).
  - The recombination lemmas are all OK.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqListGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;

open F4FPPVerifyFastPruneGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateDecomposeGoalsTheory;
open F4FPPVerifyFastParamSetListRefineGoalsTheory;
open F4FPPVerifyFastParamSetContainsRefineGoalsTheory;
open F4FPPVerifyFastParamSetContainsRefineAtlasEqGoalsTheory;
open F4FPPVerifyParamHashBridgeStateRefineGoalsTheory;

val _ = new_theory "F4FPPVerifyFastComputeBridgeDecomposeGoals";

(* Bundle (A): domain/witness accounting (purely semantic). *)
Definition fast_compute_domain_ok_def:
  fast_compute_domain_ok g <=>
    fast_domain_is_pruned g /\
    fast_witnessed_pruned g
End

Definition fast_compute_domain_ok_atlas_eq_def:
  fast_compute_domain_ok_atlas_eq g <=>
    fast_domain_is_pruned g /\
    fast_witnessed_pruned_atlas_eq g
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

(* A factored recombination lemma for the modulo-`atlas_eq` compute bundle.

   Here the ParamHash part is supplied as the *state-level* bundle (wiring +
   state_ok + exact stores_U_fast), from which we derive:
   - representation correctness modulo `atlas_eq` for `contains`, and
   - the list sound/complete obligations needed to relate `ps_list` to `U_fast`.
*)
Theorem fast_compute_domain_and_state_factored_imp_fast_compute_obligations_atlas_eq:
  !g.
    atlas_hash_eq_ok /\
    fast_compute_domain_ok_atlas_eq g /\
    paramhash_obligations_state_factored g ==>
      fast_compute_obligations_atlas_eq g
Proof
  rpt strip_tac
  \\ fs[paramhash_obligations_state_factored_def]
  \\ rw[fast_compute_obligations_atlas_eq_def]
  >- fs[fast_compute_domain_ok_atlas_eq_def]
  >- fs[fast_compute_domain_ok_atlas_eq_def]
  >- (
    (* list sound *)
    fs[paramhash_stores_U_fast_def, fast_param_set_is_paramhash_def,
       fast_param_set_list_sound_def]
  )
  >- (
    (* list complete *)
    fs[paramhash_stores_U_fast_def, fast_param_set_is_paramhash_def,
       fast_param_set_list_complete_def]
  )
  >- (
    (* contains sound modulo atlas_eq *)
    fs[fast_param_set_is_paramhash_def, fast_param_set_contains_sound_atlas_eq_def]
    \\ rpt strip_tac
    \\ `paramhash_rep_ok_atlas_eq g` by
         metis_tac[paramhash_state_ok_imp_paramhash_rep_ok_atlas_eq]
    \\ `mem_atlas_eq p (paramhash_list g)` by fs[paramhash_rep_ok_atlas_eq_def]
    \\ qpat_x_assum `mem_atlas_eq p (paramhash_list g)`
         (qx_choose_then `q` strip_assume_tac o REWRITE_RULE[mem_atlas_eq_def])
    \\ rw[mem_set_atlas_eq_def]
    \\ qexists_tac `q`
    \\ simp[]
    \\ fs[paramhash_stores_U_fast_def]
  )
  \\ (
    (* contains complete modulo atlas_eq *)
    fs[fast_param_set_is_paramhash_def, fast_param_set_contains_complete_atlas_eq_def]
    \\ rpt strip_tac
    \\ `paramhash_rep_ok_atlas_eq g` by
         metis_tac[paramhash_state_ok_imp_paramhash_rep_ok_atlas_eq]
    \\ qpat_x_assum `mem_set_atlas_eq p (U_fast g)`
         (qx_choose_then `q` strip_assume_tac o REWRITE_RULE[mem_set_atlas_eq_def])
    \\ `MEM q (paramhash_list g)` by metis_tac[paramhash_stores_U_fast_def]
    \\ `mem_atlas_eq p (paramhash_list g)` by
         (rw[mem_atlas_eq_def] \\ metis_tac[])
    \\ fs[paramhash_rep_ok_atlas_eq_def]
  )
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

Theorem fast_compute_domain_ok_imp_fast_compute_domain_ok_atlas_eq:
  !g.
    atlas_eq_equiv /\ fast_compute_domain_ok g ==> fast_compute_domain_ok_atlas_eq g
Proof
  rw[fast_compute_domain_ok_def, fast_compute_domain_ok_atlas_eq_def]
  \\ metis_tac[fast_witnessed_pruned_imp_fast_witnessed_pruned_atlas_eq]
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
