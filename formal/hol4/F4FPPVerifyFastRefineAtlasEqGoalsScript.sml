(*
  File: formal/hol4/F4FPPVerifyFastRefineAtlasEqGoalsScript.sml

  Purpose
  - Provide a version of the fast-side semantic obligations that matches the
    Atlas-facing reality: the fast computation may store parameters that are
    only semantically equal (via `atlas_eq`) to the slow semantics’ witnesses.

  Key definitions
  - `fast_witnessed_atlas_eq g`:
      every `pi ∈ U_fast g` has a witness triple `t ∈ D_fast g` whose
      `first_final_term` is unitary and semantically equal to `pi` (via
      `atlas_eq`).
  - `fast_semantic_ok_atlas_eq g`:
      `fast_witnessed_atlas_eq g ∧ fast_domain_subset g`.

  Key theorem (OK)
  - `fast_semantic_ok_atlas_eq_imp_sound_wrt_domain_atlas_eq`:
      the refined fast obligations imply the modulo-`atlas_eq` soundness notion
      used in `F4FPPVerifySpecAtlasEqGoalsTheory`.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifySpecAtlasEqGoalsTheory;
open F4FPPVerifyFastRefineGoalsTheory;

val _ = new_theory "F4FPPVerifyFastRefineAtlasEqGoals";

Definition fast_witnessed_atlas_eq_def:
  fast_witnessed_atlas_eq g <=>
    !pi.
      pi IN U_fast g ==>
        ?t pi'.
          t IN D_fast g /\
          first_final_term (mk_param g t) = SOME pi' /\
          is_unitary pi' /\
          atlas_eq pi pi'
End

Definition fast_semantic_ok_atlas_eq_def:
  fast_semantic_ok_atlas_eq g <=>
    fast_witnessed_atlas_eq g /\ fast_domain_subset g
End

Theorem fast_semantic_ok_atlas_eq_imp_sound_wrt_domain_atlas_eq:
  !g.
    fast_semantic_ok_atlas_eq g ==>
      sound_wrt_domain_atlas_eq g (D_slow g) (U_fast g)
Proof
  rw[fast_semantic_ok_atlas_eq_def, fast_witnessed_atlas_eq_def,
     fast_domain_subset_def, sound_wrt_domain_atlas_eq_def]
  \\ metis_tac[SUBSET_DEF]
QED

val _ = export_theory ();

