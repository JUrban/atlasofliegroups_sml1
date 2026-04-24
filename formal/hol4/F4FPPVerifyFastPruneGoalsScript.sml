(*
  File: formal/hol4/F4FPPVerifyFastPruneGoalsScript.sml

  Purpose
  - Refine the fast-side obligations one more step by making explicit the
    *pruning predicate* that the fast implementation uses to avoid enumerating
    the full slow domain.

  Motivation
  - In `F4_FPP_points_compute.sml`, the fast program does not iterate all
    triples in `D_slow g`; it performs key-based/bucket-based filtering and
    other admissibility tests.
  - In `F4FPPVerifyFastRefineGoalsTheory`, this is abstracted as a set
    constant `D_fast g` plus the obligation `D_fast g ⊆ D_slow g`.
  - For a proof, it is often easier to *define* a pruned domain
      { t ∈ D_slow g | fast_considers g t }
    and then show it is the domain the program actually ranges over.

  What this theory provides
  - An explicit predicate `fast_considers g t` (still abstract for now).
  - A definitional pruned domain `D_fast_pruned g`.
  - Glue lemmas that show:
      (a) if `D_fast g` equals `D_fast_pruned g`, then `fast_domain_subset g`;
      (b) if every `pi ∈ U_fast g` is witnessed by a triple satisfying
          `fast_considers`, then the original `fast_witnessed g` obligation
          holds.

  Status
  - This theory is “OK”: it is just definitional refinement and set reasoning.
    It introduces *new* obligations (`fast_domain_is_pruned`, and a pruned
    witness property) that we will eventually connect to the concrete SML.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyFastRefineGoalsTheory;
open F4FPPVerifyFastRefineAtlasEqGoalsTheory;

val _ = new_theory "F4FPPVerifyFastPruneGoals";

(* Abstract fast-side admissibility predicate:
   intended to capture *exactly* the fast program’s “consider this triple”
   decision procedure (key matching, pruning, etc.). *)
val _ = new_constant ("fast_considers", ``:group -> triple -> bool``);

Definition D_fast_pruned_def:
  D_fast_pruned g : triple set =
    {t | t IN D_slow g /\ fast_considers g t}
End

Definition fast_domain_is_pruned_def:
  fast_domain_is_pruned g <=>
    D_fast g = D_fast_pruned g
End

Definition fast_witnessed_pruned_def:
  fast_witnessed_pruned g <=>
    !pi.
      pi IN U_fast g ==>
        ?t.
          t IN D_slow g /\
          fast_considers g t /\
          first_final_term (mk_param g t) = SOME pi /\
          is_unitary pi
End

Definition fast_witnessed_pruned_atlas_eq_def:
  fast_witnessed_pruned_atlas_eq g <=>
    !pi.
      pi IN U_fast g ==>
        ?t pi'.
          t IN D_slow g /\
          fast_considers g t /\
          first_final_term (mk_param g t) = SOME pi' /\
          is_unitary pi' /\
          atlas_eq pi pi'
End

Theorem fast_domain_is_pruned_imp_fast_domain_subset:
  !g. fast_domain_is_pruned g ==> fast_domain_subset g
Proof
  rw[fast_domain_is_pruned_def, fast_domain_subset_def, D_fast_pruned_def]
  \\ simp[SUBSET_DEF]
QED

Theorem fast_domain_is_pruned_and_pruned_witness_imp_fast_witnessed:
  !g.
    fast_domain_is_pruned g /\ fast_witnessed_pruned g ==>
      fast_witnessed g
Proof
  rw[fast_domain_is_pruned_def, fast_witnessed_pruned_def, fast_witnessed_def, D_fast_pruned_def]
  \\ metis_tac[]
QED

Theorem fast_domain_is_pruned_and_pruned_witness_atlas_eq_imp_fast_witnessed_atlas_eq:
  !g.
    fast_domain_is_pruned g /\ fast_witnessed_pruned_atlas_eq g ==>
      fast_witnessed_atlas_eq g
Proof
  rw[fast_domain_is_pruned_def, fast_witnessed_pruned_atlas_eq_def,
     fast_witnessed_atlas_eq_def, D_fast_pruned_def]
  \\ metis_tac[]
QED

Theorem fast_pruned_obligations_imp_fast_semantic_obligations:
  !g.
    fast_domain_is_pruned g /\ fast_witnessed_pruned g ==>
      fast_witnessed g /\ fast_domain_subset g
Proof
  rpt gen_tac
  \\ rpt disch_tac
  \\ conj_tac
  >- metis_tac[fast_domain_is_pruned_and_pruned_witness_imp_fast_witnessed]
  \\ metis_tac[fast_domain_is_pruned_imp_fast_domain_subset]
QED

Theorem fast_pruned_obligations_imp_fast_semantic_obligations_atlas_eq:
  !g.
    fast_domain_is_pruned g /\ fast_witnessed_pruned_atlas_eq g ==>
      fast_witnessed_atlas_eq g /\ fast_domain_subset g
Proof
  rpt gen_tac
  \\ rpt disch_tac
  \\ conj_tac
  >- metis_tac[fast_domain_is_pruned_and_pruned_witness_atlas_eq_imp_fast_witnessed_atlas_eq]
  \\ metis_tac[fast_domain_is_pruned_imp_fast_domain_subset]
QED

val _ = export_theory ();
