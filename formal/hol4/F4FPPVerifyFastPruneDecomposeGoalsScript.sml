(*
  File: formal/hol4/F4FPPVerifyFastPruneDecomposeGoalsScript.sml

  Purpose
  - Further refine the fast-compute obligations introduced in
    `F4FPPVerifyFastPruneGoalsTheory` into smaller, named sub-obligations that
    are easier to connect to the concrete implementation
    `atlas-scripts-sml/F4_FPP_points_compute.sml`.

  What this file provides (all OK)
  - `fast_domain_is_pruned g` is an equality of sets.  Here we name the two
    inclusion directions explicitly:
      - `fast_domain_sound g`    : `D_fast g ⊆ D_fast_pruned g`
      - `fast_domain_complete g` : `D_fast_pruned g ⊆ D_fast g`
    and prove that together they imply `fast_domain_is_pruned g`.

  - `fast_witnessed_pruned g` bundles two kinds of facts:
      (1) every stored parameter has a witnessing triple, and
      (2) the stored parameters are unitary.
    We name these separately as:
      - `fast_witnessed_pruned_exists g`
      - `fast_unitary_set g`
    and prove they imply `fast_witnessed_pruned g`.

  Why this decomposition is useful
  - In the SML code, “which triples are considered” and “which parameters are
    inserted” are separate concerns:
      - key/bucket logic controls which `(x,lambda,gamma)` are considered,
      - construction/normalisation produces a `param`,
      - filter predicates decide whether to insert,
      - and deduplication checks may short-circuit insertion.
  - These are naturally proved as small lemmas; this theory lets later bridge
    proofs target those smaller lemmas directly.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyFastPruneGoalsTheory;

val _ = new_theory "F4FPPVerifyFastPruneDecomposeGoals";

(* ------------------------------------------------------------------------- *)
(*  Domain pruning: split equality into inclusions                             *)
(* ------------------------------------------------------------------------- *)

Definition fast_domain_sound_def:
  fast_domain_sound g <=>
    D_fast g SUBSET D_fast_pruned g
End

Definition fast_domain_complete_def:
  fast_domain_complete g <=>
    D_fast_pruned g SUBSET D_fast g
End

Theorem fast_domain_sound_and_complete_imp_fast_domain_is_pruned:
  !g.
    fast_domain_sound g /\ fast_domain_complete g ==>
      fast_domain_is_pruned g
Proof
  rw[fast_domain_sound_def, fast_domain_complete_def, fast_domain_is_pruned_def]
  \\ metis_tac[SUBSET_ANTISYM_EQ]
QED

(* ------------------------------------------------------------------------- *)
(*  Witnessing: split existence and unitarity                                 *)
(* ------------------------------------------------------------------------- *)

Definition fast_witnessed_pruned_exists_def:
  fast_witnessed_pruned_exists g <=>
    !pi.
      pi IN U_fast g ==>
        ?t.
          t IN D_slow g /\
          fast_considers g t /\
          first_final_term (mk_param g t) = SOME pi
End

Definition fast_unitary_set_def:
  fast_unitary_set g <=>
    !pi. pi IN U_fast g ==> is_unitary pi
End

Theorem fast_witnessed_pruned_exists_and_unitary_imp_fast_witnessed_pruned:
  !g.
    fast_witnessed_pruned_exists g /\ fast_unitary_set g ==>
      fast_witnessed_pruned g
Proof
  rw[fast_witnessed_pruned_exists_def, fast_unitary_set_def, fast_witnessed_pruned_def]
  \\ metis_tac[]
QED

val _ = export_theory ();
