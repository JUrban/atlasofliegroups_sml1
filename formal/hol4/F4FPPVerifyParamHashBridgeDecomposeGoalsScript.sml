 (*
  File: formal/hol4/F4FPPVerifyParamHashBridgeDecomposeGoalsScript.sml

  Purpose
  - Make the ParamHash obligations more refinement-friendly by splitting the
    current bundle `paramhash_ok g` into two conceptually independent parts:

      (A) data-structure representation correctness:
            `paramhash_rep_ok g`
          stating that `paramhash_contains g` agrees with membership in the
          list `paramhash_list g` (i.e. no false positives/negatives relative
          to the enumerated contents), and

      (B) semantic agreement with the goal-layer set:
            `paramhash_stores_U_fast g`
          stating that the enumerated list represents exactly `U_fast g`.

  Motivation
  - When we eventually replace `cheat`ed bridge lemmas by real arguments, we
    want to prove (A) via a generic hash-table correctness proof (CakeML), and
    prove (B) via a computation/algorithm argument about what the fast phase
    inserts.
  - The existing `paramhash_ok` bundles (A) and (B) together via
    sound/complete facts “w.r.t. `U_fast`”, which makes it harder to re-use a
    standalone data-structure proof.

  Status
  - Definitions and implication lemmas are OK.
  - “execution success ⇒ obligations” bridge lemmas are isolated in
    `F4FPPVerifyParamHashBridgeDecomposeCheatsGoalsTheory`, to keep this theory
    entirely OK.
 *)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqListGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;
open F4FPPVerifyParamHashBridgeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeDecomposeGoals";

Theorem IN_set_MEM[simp]:
  !x xs. x IN set xs <=> MEM x xs
Proof
  Induct_on `xs` \\ simp[]
QED

(* (A) Data-structure correctness: `contains` agrees with membership in `list`. *)
Definition paramhash_rep_ok_def:
  paramhash_rep_ok g <=>
    !p. paramhash_contains g p <=> MEM p (paramhash_list g)
End

(* More realistic variant: membership is modulo Atlas's semantic equality. *)
Definition paramhash_rep_ok_atlas_eq_def:
  paramhash_rep_ok_atlas_eq g <=>
    !p. paramhash_contains g p <=> mem_atlas_eq p (paramhash_list g)
End

Theorem paramhash_rep_ok_atlas_eq_iff_mem_set_atlas_eq:
  !g.
    paramhash_rep_ok_atlas_eq g <=>
      !p. paramhash_contains g p <=> mem_set_atlas_eq p (set (paramhash_list g))
Proof
  rw[paramhash_rep_ok_atlas_eq_def, mem_atlas_eq_iff_mem_set_atlas_eq_set]
QED

Theorem atlas_eq_is_hol_eq_and_paramhash_rep_ok_atlas_eq_imp_paramhash_rep_ok:
  !g. atlas_eq_is_hol_eq /\ paramhash_rep_ok_atlas_eq g ==> paramhash_rep_ok g
Proof
  rw[paramhash_rep_ok_def, paramhash_rep_ok_atlas_eq_def]
  \\ metis_tac[atlas_eq_is_hol_eq_imp_mem_atlas_eq_eq_MEM]
QED

(* (B) Semantic agreement: the stored contents are exactly the fast set. *)
Definition paramhash_stores_U_fast_def:
  paramhash_stores_U_fast g <=>
    !p. p IN U_fast g <=> MEM p (paramhash_list g)
End

(* More realistic semantic agreement: equality of represented sets modulo `atlas_eq`. *)
Definition paramhash_stores_U_fast_atlas_eq_def:
  paramhash_stores_U_fast_atlas_eq g <=>
    set_atlas_eq (U_fast g) (set (paramhash_list g))
End

Theorem atlas_eq_is_hol_eq_and_paramhash_stores_U_fast_imp_atlas_eq:
  !g.
    atlas_eq_is_hol_eq /\ paramhash_stores_U_fast g ==>
      paramhash_stores_U_fast_atlas_eq g
Proof
  rpt gen_tac
  \\ strip_tac
  \\ rw[paramhash_stores_U_fast_atlas_eq_def, set_atlas_eq_def]
  \\ fs[paramhash_stores_U_fast_def]
  \\ simp[atlas_eq_is_hol_eq_imp_mem_set_atlas_eq_eq_IN, atlas_eq_is_hol_eq_imp_mem_atlas_eq_eq_MEM]
QED

(* Convenience bundle: ParamHash correctness obligations stated modulo `atlas_eq`. *)
Definition paramhash_obligations_factored_atlas_eq_def:
  paramhash_obligations_factored_atlas_eq g <=>
    fast_param_set_is_paramhash g /\
    paramhash_rep_ok_atlas_eq g /\
    paramhash_stores_U_fast_atlas_eq g
End

Theorem paramhash_rep_ok_and_stores_U_fast_imp_paramhash_ok:
  !g.
    fast_param_set_is_paramhash g /\
    paramhash_rep_ok g /\
    paramhash_stores_U_fast g ==>
      paramhash_ok g
Proof
  rpt gen_tac
  \\ disch_then strip_assume_tac
  \\ rw
      [ paramhash_ok_def
      , paramhash_list_sound_def
      , paramhash_list_complete_def
      , paramhash_contains_sound_def
      , paramhash_contains_complete_def
      ]
  \\ fs[paramhash_rep_ok_def, paramhash_stores_U_fast_def]
  \\ metis_tac[]
QED

(* A slightly more convenient “no mention of `paramhash_ok`” bundle that matches
   how we expect to split future proofs. *)
Definition paramhash_obligations_factored_def:
  paramhash_obligations_factored g <=>
    fast_param_set_is_paramhash g /\
    paramhash_rep_ok g /\
    paramhash_stores_U_fast g
End

Theorem paramhash_obligations_factored_imp_paramhash_ok:
  !g. paramhash_obligations_factored g ==> paramhash_ok g
Proof
  rw[paramhash_obligations_factored_def]
  \\ metis_tac[paramhash_rep_ok_and_stores_U_fast_imp_paramhash_ok]
QED

Theorem atlas_eq_is_hol_eq_and_paramhash_obligations_factored_atlas_eq_imp_factored:
  !g.
    atlas_eq_is_hol_eq /\ paramhash_obligations_factored_atlas_eq g ==>
      paramhash_obligations_factored g
Proof
  rpt gen_tac
  \\ strip_tac
  \\ fs[]
  \\ simp[paramhash_obligations_factored_def]
  \\ conj_tac >- fs[paramhash_obligations_factored_atlas_eq_def]
  \\ conj_tac >- (
    fs[paramhash_obligations_factored_atlas_eq_def]
    \\ metis_tac[atlas_eq_is_hol_eq_and_paramhash_rep_ok_atlas_eq_imp_paramhash_rep_ok]
  )
  \\ simp[paramhash_stores_U_fast_def]
  \\ gen_tac
  \\ fs[paramhash_obligations_factored_atlas_eq_def]
  \\ `U_fast g = set (paramhash_list g)` by (
       fs[paramhash_stores_U_fast_atlas_eq_def]
       \\ drule atlas_eq_is_hol_eq_imp_set_atlas_eq_eq_eq
       \\ disch_then (qspecl_then [`U_fast g`, `set (paramhash_list g)`] mp_tac)
       \\ simp[]
     )
  \\ fs[]
QED

(* Bridge lemmas from `fast_compute_program_succeeds` to these obligations are
   recorded in `F4FPPVerifyParamHashBridgeDecomposeCheatsGoalsTheory`. *)

val _ = export_theory ();
