(*
  File: formal/hol4/F4FPPVerifyAtlasFFIContractsCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) “contracts hold” assumption for the Atlas
    FFI layer.

  Rationale
  - `F4FPPVerifyAtlasFFIContractsGoalsTheory` defines the contract predicates
    and proves small internal consequences; it should remain “OK”.
  - The statement that the *concrete* Atlas/FFI implementation satisfies these
    contracts is a project-level trust decision, so it lives in this dedicated
    `*Cheats*` theory.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;

val _ = new_theory "F4FPPVerifyAtlasFFIContractsCheatsGoals";

Theorem atlas_ffi_contracts_hold:
  atlas_ffi_contracts
Proof
  cheat
QED

val _ = export_theory ();

