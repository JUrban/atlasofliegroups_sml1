(*
  File: formal/hol4/F4FPPVerifySlowProgramDecomposeBridgeCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bridge obligations from
    `slow_program_succeeds g` to the smaller predicates introduced in
    `F4FPPVerifySlowProgramDecomposeBridgeGoalsTheory`.

  Rationale
  - `F4FPPVerifySlowProgramDecomposeBridgeGoalsTheory` is an “OK” decomposition
    of the slow checker into abstract components.
  - The connection to concrete Poly/ML execution (I/O, loops, and Atlas/FFI
    calls) is the expected CakeML/FFI proof surface, so it lives here.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifySMLBridgeGoalsTheory;
open F4FPPVerifySlowProgramDecomposeBridgeGoalsTheory;

val _ = new_theory "F4FPPVerifySlowProgramDecomposeBridgeCheatsGoals";

Theorem slow_program_succeeds_imp_slow_missing_ok:
  !g. slow_program_succeeds g ==> slow_missing_ok g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - show SML’s `triple_is_missing` matches `missing_witness`:
        - the parameter construction matches `mk_param`,
        - the first-final-term operation matches `first_final_term`,
        - and the unitary/membership tests match `is_unitary` and `U_fast`.
  *)
  cheat
QED

Theorem slow_program_succeeds_imp_slow_domain_list_is_components:
  !g. slow_program_succeeds g ==> slow_domain_list_is_components g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - show SML’s `for_domain` nested loops enumerate exactly
      `dom_list_from_components g` (or at least the same set, if order is not
      used).
  *)
  cheat
QED

Theorem slow_program_succeeds_imp_slow_ok_sml:
  !g. slow_program_succeeds g ==> slow_ok_sml g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - show the SML miss counter equals `LENGTH (FILTER slow_missing ...)`,
      and that “success” means it printed/returned 0 misses.
  *)
  cheat
QED

Theorem slow_program_succeeds_imp_slow_missing_ok_atlas_eq:
  !g. slow_program_succeeds g ==> slow_missing_ok_atlas_eq g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - show SML’s “missing witness?” test corresponds to
      `missing_witness_atlas_eq` rather than `missing_witness`, i.e. that its
      membership test in the fast set is modulo `atlas_eq`.
  *)
  cheat
QED

Theorem slow_program_succeeds_imp_slow_ok_sml_atlas_eq:
  !g. slow_program_succeeds g ==> slow_ok_sml_atlas_eq g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - show the SML miss counter equals
        `LENGTH (FILTER (slow_missing_atlas_eq g) (slow_domain_list g))`,
      and that “success” means it observed 0 misses.
  *)
  cheat
QED

(* Derived bridges: earlier “slow success ⇒ slow_ok_components”, now routed
   through smaller explicit obligations. *)
Theorem slow_program_succeeds_imp_slow_ok_components_decomposed:
  !g. slow_program_succeeds g ==> slow_ok_components g
Proof
  rpt strip_tac
  \\ match_mp_tac slow_bridge_obligations_imp_slow_ok_components
  \\ metis_tac
       [ slow_program_succeeds_imp_slow_missing_ok
       , slow_program_succeeds_imp_slow_domain_list_is_components
       , slow_program_succeeds_imp_slow_ok_sml
       ]
QED

Theorem slow_program_succeeds_imp_slow_ok_components_atlas_eq_decomposed:
  !g. slow_program_succeeds g ==> slow_ok_components_atlas_eq g
Proof
  rpt strip_tac
  \\ match_mp_tac slow_bridge_obligations_imp_slow_ok_components_atlas_eq
  \\ metis_tac
       [ slow_program_succeeds_imp_slow_missing_ok_atlas_eq
       , slow_program_succeeds_imp_slow_domain_list_is_components
       , slow_program_succeeds_imp_slow_ok_sml_atlas_eq
       ]
QED

val _ = export_theory ();

