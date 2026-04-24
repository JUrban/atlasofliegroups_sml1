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
  - “program success ⇒ obligations” lemmas are recorded but `cheat`ed.
 *)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifyGoalsTheory;
open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyParamHashBridgeGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeDecomposeGoals";

(* (A) Data-structure correctness: `contains` agrees with membership in `list`. *)
Definition paramhash_rep_ok_def:
  paramhash_rep_ok g <=>
    !p. paramhash_contains g p <=> MEM p (paramhash_list g)
End

(* (B) Semantic agreement: the stored contents are exactly the fast set. *)
Definition paramhash_stores_U_fast_def:
  paramhash_stores_U_fast g <=>
    !p. p IN U_fast g <=> MEM p (paramhash_list g)
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

(* --- Bridge from compute-phase success (currently CHEATED) --- *)

Theorem fast_compute_program_succeeds_imp_paramhash_rep_ok:
  !g. fast_compute_program_succeeds g ==> paramhash_rep_ok g
Proof
  (*
    Intended proof (later, without `cheat`):
    - relate the concrete SML `ParamHash.contains` and `ParamHash.list`
      observations for the post-state of `computeAllIntoParamHash`.
    - prove `contains p <=> MEM p (list())` for that post-state.
    - discharge any FFI coherence obligations needed for hashing/equality.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_stores_U_fast:
  !g. fast_compute_program_succeeds g ==> paramhash_stores_U_fast g
Proof
  (*
    Intended proof (later, without `cheat`):
    - show the fast compute phase inserts *exactly* the parameters that
      constitute `U_fast g` (as defined in the goal layer).
    - this is an algorithmic/semantic argument, independent of hash-table
      representation.
  *)
  cheat
QED

Theorem fast_compute_program_succeeds_imp_paramhash_obligations_factored:
  !g. fast_compute_program_succeeds g ==> paramhash_obligations_factored g
Proof
  rpt strip_tac
  \\ rw[paramhash_obligations_factored_def]
  >- (
    (* In the current development, “wiring” is part of `paramhash_ok` rather than
       being derived from execution; we record it as a future obligation. *)
    cheat )
  >- metis_tac[fast_compute_program_succeeds_imp_paramhash_rep_ok]
  \\ metis_tac[fast_compute_program_succeeds_imp_paramhash_stores_U_fast]
QED

val _ = export_theory ();
