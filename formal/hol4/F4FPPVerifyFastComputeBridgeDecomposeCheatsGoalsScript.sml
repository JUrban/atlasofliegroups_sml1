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
open F4FPPVerifyFastPruneDecomposeGoalsTheory;
open F4FPPVerifyFastPruneTraceGoalsTheory;
open F4FPPVerifyFastPruneTraceCheatsGoalsTheory;
open F4FPPVerifyFastWitnessTraceGoalsTheory;
open F4FPPVerifyFastWitnessTraceCheatsGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeCheatsGoalsTheory;

val _ = new_theory "F4FPPVerifyFastComputeBridgeDecomposeCheatsGoals";

(* Smaller bridge obligations for the domain/witness part. *)

Theorem fast_compute_program_succeeds_imp_fast_domain_sound:
  !g. fast_compute_program_succeeds g ==> fast_domain_sound g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - identify the exact pruning predicate (`fast_considers`) implemented by
      `F4_FPP_points_compute` (bucket/key matching + additional filters),
    - show the fast enumeration never considers triples outside the pruned set.
  *)
  rpt strip_tac
  \\ match_mp_tac fast_domain_trace_ok_and_trace_sound_imp_fast_domain_sound
  \\ metis_tac
       [ fast_compute_program_succeeds_imp_fast_domain_trace_ok
       , fast_compute_program_succeeds_imp_fast_domain_trace_sound
       ]
QED

Theorem fast_compute_program_succeeds_imp_fast_domain_complete:
  !g. fast_compute_program_succeeds g ==> fast_domain_complete g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - show every triple passing the pruning predicate is covered by the
      enumeration logic (bucket/key matching completeness).
  *)
  rpt strip_tac
  \\ match_mp_tac fast_domain_trace_ok_and_trace_complete_imp_fast_domain_complete
  \\ metis_tac
       [ fast_compute_program_succeeds_imp_fast_domain_trace_ok
       , fast_compute_program_succeeds_imp_fast_domain_trace_complete
       ]
QED

Theorem fast_compute_program_succeeds_imp_fast_domain_is_pruned:
  !g. fast_compute_program_succeeds g ==> fast_domain_is_pruned g
Proof
  rpt strip_tac
  \\ match_mp_tac fast_domain_sound_and_complete_imp_fast_domain_is_pruned
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_fast_domain_sound
      , fast_compute_program_succeeds_imp_fast_domain_complete
      ]
QED

Theorem fast_compute_program_succeeds_imp_fast_witnessed_pruned_exists:
  !g. fast_compute_program_succeeds g ==> fast_witnessed_pruned_exists g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - show each stored element was inserted due to some witness triple that
      passed pruning and produced that final parameter.
  *)
  rpt strip_tac
  \\ match_mp_tac fast_insert_trace_sound_and_covers_imp_fast_witnessed_pruned_exists
  \\ metis_tac
       [ fast_compute_program_succeeds_imp_fast_insert_trace_sound
       , fast_compute_program_succeeds_imp_fast_insert_trace_covers_U_fast
       ]
QED

Theorem fast_compute_program_succeeds_imp_fast_unitary_set:
  !g. fast_compute_program_succeeds g ==> fast_unitary_set g
Proof
  (*
    Intended proof ingredients (later, without `cheat`):
    - relate the SML filter `atlas_param_is_unitary` (under the flag setting
      used by `VerifyF4FPP`) to the abstract predicate `is_unitary`.
  *)
  rpt strip_tac
  \\ match_mp_tac fast_insert_trace_unitary_and_covers_imp_fast_unitary_set
  \\ metis_tac
       [ fast_compute_program_succeeds_imp_fast_insert_trace_unitary
       , fast_compute_program_succeeds_imp_fast_insert_trace_covers_U_fast
       ]
QED

Theorem fast_compute_program_succeeds_imp_fast_witnessed_pruned:
  !g. fast_compute_program_succeeds g ==> fast_witnessed_pruned g
Proof
  rpt strip_tac
  \\ match_mp_tac fast_witnessed_pruned_exists_and_unitary_imp_fast_witnessed_pruned
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_fast_witnessed_pruned_exists
      , fast_compute_program_succeeds_imp_fast_unitary_set
      ]
QED

Theorem fast_compute_program_succeeds_imp_fast_compute_domain_ok:
  !g. fast_compute_program_succeeds g ==> fast_compute_domain_ok g
Proof
  rpt strip_tac
  \\ rw[fast_compute_domain_ok_def]
  \\ metis_tac
      [ fast_compute_program_succeeds_imp_fast_domain_is_pruned
      , fast_compute_program_succeeds_imp_fast_witnessed_pruned
      ]
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
