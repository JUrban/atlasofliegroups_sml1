(*
  File: formal/hol4/F4FPPVerifyAlgScript.sml

  Purpose
  - Algorithmic “skeleton” for the slow checker, expressed purely over lists,
    and basic lemmas connecting the integer miss-count to the logical
    `missing_witness` predicate from `F4FPPVerifySpec`.

  This is the next step in the VERIFY_ESTIMATE Stage A approach:
  prove correctness of the ML control-flow (fold over a domain) under abstract
  assumptions about Atlas/C++ primitives.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open arithmeticTheory;
open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;

val _ = new_theory "F4FPPVerifyAlg";

Definition check_domain_fun_def:
  (check_domain_fun (g:group) (U_fast:param set) ([]:triple list) = 0) /\
  (check_domain_fun g U_fast (t::ts) =
     (if missing_witness g U_fast t then 1 else 0) + check_domain_fun g U_fast ts)
End

Theorem check_domain_fun_length_filter:
  !g U_fast ts.
    check_domain_fun g U_fast ts =
    LENGTH (FILTER (missing_witness g U_fast) ts)
Proof
  Induct_on `ts` >>
  simp [check_domain_fun_def] >>
  Cases_on `missing_witness g U_fast h` >>
  simp []
QED

Theorem check_domain_fun_eq0_iff:
  !g U_fast ts.
    check_domain_fun g U_fast ts = 0 <=>
    (!t. MEM t ts ==> ~missing_witness g U_fast t)
Proof
  (* Now use standard list lemmas: LENGTH=0 <-> FILTER=[] <-> EVERY (~missing). *)
  simp [check_domain_fun_length_filter, LENGTH_EQ_0, FILTER_EQ_NIL, EVERY_MEM]
QED

Theorem check_domain_fun_eq0_imp_complete_rel_list:
  !g U_fast ts.
    check_domain_fun g U_fast ts = 0 ==>
    complete_rel g (set ts) U_fast
Proof
  rw [complete_rel_iff_no_missing] >>
  metis_tac [check_domain_fun_eq0_iff]
QED

val _ = export_theory ();
