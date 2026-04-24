(*
  File: formal/hol4/F4FPPVerifyAlgAtlasEqGoalsScript.sml

  Purpose
  - Provide the slow-checker list-fold “algorithmic skeleton” in a form that is
    faithful to the Atlas-facing reality: membership is tested modulo the Atlas
    semantic equality `atlas_eq`.

  Concretely:
  - `check_domain_fun_atlas_eq` counts “misses” where the triple witnesses a
    unitary final term not represented (up to `atlas_eq`) in `U_fast`.
  - The main lemma mirrors `F4FPPVerifyAlgTheory`:

      `check_domain_fun_atlas_eq ... = 0  ==>  complete_rel_atlas_eq ...`

  Status
  - All results are “OK” (pure list reasoning). The remaining work is to relate
    the concrete slow SML program’s control-flow to `check_domain_fun_atlas_eq`.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open arithmeticTheory;
open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifySpecAtlasEqGoalsTheory;

val _ = new_theory "F4FPPVerifyAlgAtlasEqGoals";

Definition check_domain_fun_atlas_eq_def:
  (check_domain_fun_atlas_eq (g:group) (U_fast:param set) ([]:triple list) = 0) /\
  (check_domain_fun_atlas_eq g U_fast (t::ts) =
     (if missing_witness_atlas_eq g U_fast t then 1 else 0) +
     check_domain_fun_atlas_eq g U_fast ts)
End

Theorem check_domain_fun_atlas_eq_length_filter:
  !g U_fast ts.
    check_domain_fun_atlas_eq g U_fast ts =
    LENGTH (FILTER (missing_witness_atlas_eq g U_fast) ts)
Proof
  Induct_on `ts`
  \\ simp[check_domain_fun_atlas_eq_def]
  \\ Cases_on `missing_witness_atlas_eq g U_fast h`
  \\ simp[]
QED

Theorem check_domain_fun_atlas_eq_eq0_iff:
  !g U_fast ts.
    check_domain_fun_atlas_eq g U_fast ts = 0 <=>
    (!t. MEM t ts ==> ~missing_witness_atlas_eq g U_fast t)
Proof
  simp
    [ check_domain_fun_atlas_eq_length_filter
    , LENGTH_EQ_0
    , FILTER_EQ_NIL
    , EVERY_MEM
    ]
QED

Theorem check_domain_fun_atlas_eq_eq0_imp_complete_rel_atlas_eq_list:
  !g U_fast ts.
    check_domain_fun_atlas_eq g U_fast ts = 0 ==>
      complete_rel_atlas_eq g (set ts) U_fast
Proof
  rw[complete_rel_atlas_eq_iff_no_missing]
  \\ metis_tac[check_domain_fun_atlas_eq_eq0_iff]
QED

val _ = export_theory ();

