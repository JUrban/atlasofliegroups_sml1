(*
  File: formal/hol4/F4FPPVerifyFastComputeBridgeDecomposeCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bridge theorems that connect fast compute
    phase success (`fast_compute_program_succeeds`) to the *factored* compute
    obligation bundles introduced in
    `F4FPPVerifyFastComputeBridgeDecomposeGoalsTheory`.

  Rationale
  - `F4FPPVerifyFastComputeBridgeDecomposeGoalsTheory` is intended to be an “OK”
    theory of definitions + logical recombination lemmas. It should not itself
    contain any `cheat`.
  - The concrete connection “the SML compute phase returned successfully ⇒
    these obligations hold” is a program-semantics/FFI proof obligation; we
    keep those statements in this dedicated `*Cheats*` theory so their
    CHEAT-taint does not spread unnecessarily.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyFastComputeBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeCheatsGoalsTheory;

val _ = new_theory "F4FPPVerifyFastComputeBridgeDecomposeCheatsGoals";

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

