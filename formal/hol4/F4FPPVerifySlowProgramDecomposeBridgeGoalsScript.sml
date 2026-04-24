(*
  File: formal/hol4/F4FPPVerifySlowProgramDecomposeBridgeGoalsScript.sml

  Purpose
  - Decompose the slow program’s bridge obligations into smaller pieces that
    match the concrete structure of `atlas-scripts-sml/SimplerVerifyF4FPP.sml`:

      - a nested-loop enumerator of triples (x,lambda,gamma),
      - a per-triple “is missing witness?” predicate,
      - and a counter / “0 misses” success condition.

  Motivation
  - In earlier bridge layers we used a single cheated lemma stating:
      `slow_program_succeeds g ⇒ slow_ok_components g`.
  - This file breaks that into three explicit bridge obligations, so that later
    we can prove each part separately (and potentially using CakeML proofs for
    list/loop structure plus FFI specs for Atlas calls).

  Status
  - The bridge premises from `slow_program_succeeds` are currently `cheat`ed.
  - The composition lemma deriving `slow_ok_components` is “OK”.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyAlgTheory;
open F4FPPVerifySpecAtlasEqGoalsTheory;
open F4FPPVerifyAlgAtlasEqGoalsTheory;
open F4FPPVerifySlowRefineGoalsTheory;
open F4FPPVerifySlowRefineAtlasEqGoalsTheory;
open F4FPPVerifyDomainGoalsTheory;
open F4FPPVerifySMLBridgeGoalsTheory;

val _ = new_theory "F4FPPVerifySlowProgramDecomposeBridgeGoals";

(* Abstract list representing the exact triple enumeration order used by the
   slow SML program (if we were to materialize its `for_domain` loop). *)
val _ = new_constant ("slow_domain_list", ``:group -> triple list``);

(* Abstract predicate representing the slow SML program’s `triple_is_missing`
   test (relative to the fast set `U_fast g`). *)
val _ = new_constant ("slow_missing", ``:group -> triple -> bool``);

(* More realistic (Atlas-facing) version: the slow program’s missing predicate
   uses Atlas semantic equality `atlas_eq` for membership in the fast set. *)
val _ = new_constant ("slow_missing_atlas_eq", ``:group -> triple -> bool``);

Definition slow_missing_ok_def:
  slow_missing_ok g <=>
    !t. slow_missing g t <=> missing_witness g (U_fast g) t
End

Definition slow_missing_ok_atlas_eq_def:
  slow_missing_ok_atlas_eq g <=>
    !t. slow_missing_atlas_eq g t <=> missing_witness_atlas_eq g (U_fast g) t
End

Definition slow_domain_list_is_components_def:
  slow_domain_list_is_components g <=>
    slow_domain_list g = dom_list_from_components g
End

(* “0 misses” success condition expressed via the slow predicate and list. *)
Definition slow_ok_sml_def:
  slow_ok_sml g <=>
    LENGTH (FILTER (slow_missing g) (slow_domain_list g)) = 0
End

Definition slow_ok_sml_atlas_eq_def:
  slow_ok_sml_atlas_eq g <=>
    LENGTH (FILTER (slow_missing_atlas_eq g) (slow_domain_list g)) = 0
End

(* OK: show that `slow_ok_sml` plus `slow_missing_ok` implies the list-model
   checker reports 0 misses on the slow list. *)
Theorem slow_ok_sml_and_missing_ok_imp_check_domain_fun_eq0:
  !g.
    slow_missing_ok g /\ slow_ok_sml g ==>
      check_domain_fun g (U_fast g) (slow_domain_list g) = 0
Proof
  rpt strip_tac
  \\ `EVERY (\x. ~(slow_missing g x)) (slow_domain_list g)` by
       (fs[slow_ok_sml_def, LENGTH_EQ_0] \\ metis_tac[FILTER_EQ_NIL])
  \\ `!t. MEM t (slow_domain_list g) ==> ~slow_missing g t` by
       fs[EVERY_MEM]
  \\ `!t. MEM t (slow_domain_list g) ==> ~missing_witness g (U_fast g) t` by
       (fs[slow_missing_ok_def] \\ metis_tac[])
  \\ metis_tac[check_domain_fun_eq0_iff]
QED

(* OK: the three smaller obligations imply the main slow-side goal predicate. *)
Theorem slow_bridge_obligations_imp_slow_ok_components:
  !g.
    slow_missing_ok g /\
    slow_domain_list_is_components g /\
    slow_ok_sml g ==>
      slow_ok_components g
Proof
  rpt strip_tac
  \\ fs[slow_domain_list_is_components_def, slow_ok_components_def]
  \\ metis_tac[slow_ok_sml_and_missing_ok_imp_check_domain_fun_eq0]
QED

Theorem slow_ok_sml_and_missing_ok_atlas_eq_imp_check_domain_fun_atlas_eq_eq0:
  !g.
    slow_missing_ok_atlas_eq g /\ slow_ok_sml_atlas_eq g ==>
      check_domain_fun_atlas_eq g (U_fast g) (slow_domain_list g) = 0
Proof
  rpt strip_tac
  \\ `EVERY (\x. ~(slow_missing_atlas_eq g x)) (slow_domain_list g)` by
       (fs[slow_ok_sml_atlas_eq_def, LENGTH_EQ_0] \\ metis_tac[FILTER_EQ_NIL])
  \\ `!t. MEM t (slow_domain_list g) ==> ~slow_missing_atlas_eq g t` by
       fs[EVERY_MEM]
  \\ `!t. MEM t (slow_domain_list g) ==> ~missing_witness_atlas_eq g (U_fast g) t` by
       (fs[slow_missing_ok_atlas_eq_def] \\ metis_tac[])
  \\ metis_tac[check_domain_fun_atlas_eq_eq0_iff]
QED

Theorem slow_bridge_obligations_imp_slow_ok_components_atlas_eq:
  !g.
    slow_missing_ok_atlas_eq g /\
    slow_domain_list_is_components g /\
    slow_ok_sml_atlas_eq g ==>
      slow_ok_components_atlas_eq g
Proof
  rpt strip_tac
  \\ fs[slow_domain_list_is_components_def, slow_ok_components_atlas_eq_def]
  \\ metis_tac[slow_ok_sml_and_missing_ok_atlas_eq_imp_check_domain_fun_atlas_eq_eq0]
QED

(* --- Bridge obligations from `slow_program_succeeds` (currently CHEATED) --- *)

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

(* Derived bridge: this is the earlier “slow success ⇒ slow_ok_components” but
   now factored through smaller, explicit obligations. *)
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
