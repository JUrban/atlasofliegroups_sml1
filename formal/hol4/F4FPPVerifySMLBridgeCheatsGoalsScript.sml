(*
  File: formal/hol4/F4FPPVerifySMLBridgeCheatsGoalsScript.sml

  Purpose
  - Isolate the (currently `cheat`ed) bridge theorems connecting the abstract
    “program success” predicates from `F4FPPVerifySMLBridgeGoalsTheory` to the
    high-level goal predicates (`fast_ok`, `slow_ok`, `full_ok`).

  Rationale
  - The corresponding `*Goals*` theory only introduces the abstract success
    predicates; it should remain a lightweight, definition-only interface.
  - These bridge lemmas are intended to be discharged later by:
      - CakeML proofs about evaluation/control-flow for the SML programs, and
      - explicit Atlas/FFI contracts for the semantic primitives.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyFullGoalsTheory;
open F4FPPBottomLayerGoalsTheory;
open F4FPPVerifySMLBridgeGoalsTheory;

val _ = new_theory "F4FPPVerifySMLBridgeCheatsGoals";

(* Refinement obligations: success implies the abstract goal predicates. *)
Theorem fast_program_succeeds_imp_fast_ok:
  !g dirac. fast_program_succeeds g dirac ==> fast_ok g dirac
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - relate `VerifyF4FPP.sml` to the fast compute + bottom-layer bridges
      (or to the refined obligation bundles directly).
  *)
  cheat
QED

Theorem slow_program_succeeds_imp_dom_and_slow_ok:
  !g. slow_program_succeeds g ==> dom_list_correct g /\ slow_ok g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - relate `SimplerVerifyF4FPP.sml` to the slow program decomposition bridges
      (`slow_domain_list`, `slow_missing`, “0 misses”), and to `dom_list_correct`.
  *)
  cheat
QED

Theorem slow_program_succeeds_imp_dom_list_correct:
  !g. slow_program_succeeds g ==> dom_list_correct g
Proof
  metis_tac[slow_program_succeeds_imp_dom_and_slow_ok]
QED

Theorem slow_program_succeeds_imp_slow_ok:
  !g. slow_program_succeeds g ==> slow_ok g
Proof
  metis_tac[slow_program_succeeds_imp_dom_and_slow_ok]
QED

Theorem fast_and_slow_programs_succeed_imp_full_ok:
  !g dirac.
    fast_program_succeeds g dirac /\ slow_program_succeeds g ==> full_ok g dirac
Proof
  rpt strip_tac
  \\ pop_assum strip_assume_tac
  \\ simp[full_ok_def]
  \\ rpt conj_tac
  \\ metis_tac
      [ slow_program_succeeds_imp_dom_list_correct
      , slow_program_succeeds_imp_slow_ok
      , fast_program_succeeds_imp_fast_ok
      ]
  \\ metis_tac
      [ slow_program_succeeds_imp_dom_list_correct
      , slow_program_succeeds_imp_slow_ok
      , fast_program_succeeds_imp_fast_ok
      ]
  \\ metis_tac
      [ slow_program_succeeds_imp_dom_list_correct
      , slow_program_succeeds_imp_slow_ok
      , fast_program_succeeds_imp_fast_ok
      ]
QED

Theorem fast_and_slow_programs_succeed_gives_equivalence:
  !g dirac.
    fast_program_succeeds g dirac /\ slow_program_succeeds g ==>
      U_slow g (D_slow g) = U_fast g /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  rpt strip_tac
  \\ mp_tac (SPEC_ALL fast_and_slow_programs_succeed_imp_full_ok)
  \\ impl_tac >- metis_tac[]
  \\ strip_tac
  \\ metis_tac[full_ok_implies_set_equality, full_ok_implies_bottom_layer_total_ok]
QED

val _ = export_theory ();
