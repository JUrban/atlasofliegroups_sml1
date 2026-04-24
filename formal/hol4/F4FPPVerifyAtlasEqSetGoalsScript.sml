(*
  File: formal/hol4/F4FPPVerifyAtlasEqSetGoalsScript.sml

  Purpose
  - Provide a small “setoid vocabulary” for reasoning about sets of `param`
    up to Atlas semantic equality `atlas_eq`, without assuming
    `atlas_eq_is_hol_eq`.

  Motivation
  - The end-to-end equivalence story for the fast/slow verifiers is naturally
    stated modulo `atlas_eq`:
      two implementations may return different representatives, but the same
      semantic objects.
  - This theory introduces:
      - membership modulo equality (`mem_set_atlas_eq`),
      - equality modulo equality (`set_atlas_eq`),
      - the closure operation (`atlas_eq_closure`),
    and relates them to the list-level predicate `mem_atlas_eq`.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqListGoalsTheory;

val _ = new_theory "F4FPPVerifyAtlasEqSetGoals";

(* Membership in a set modulo Atlas equality. *)
Definition mem_set_atlas_eq_def:
  mem_set_atlas_eq (p:param) (U:param set) <=> ?q. q IN U /\ atlas_eq p q
End

(* Extensional equality of sets modulo Atlas equality. *)
Definition set_atlas_eq_def:
  set_atlas_eq (U:param set) (V:param set) <=>
    !p. mem_set_atlas_eq p U <=> mem_set_atlas_eq p V
End

(* Closure of a set under Atlas equality (i.e. quotient-style extensional view). *)
Definition atlas_eq_closure_def:
  atlas_eq_closure (U:param set) : param set = {p | mem_set_atlas_eq p U}
End

Theorem mem_atlas_eq_iff_mem_set_atlas_eq_set:
  !p xs. mem_atlas_eq p xs <=> mem_set_atlas_eq p (set xs)
Proof
  rw[mem_atlas_eq_def, mem_set_atlas_eq_def]
QED

Theorem atlas_eq_is_hol_eq_imp_mem_set_atlas_eq_eq_IN:
  atlas_eq_is_hol_eq ==> !p U. mem_set_atlas_eq p U <=> p IN U
Proof
  rw[mem_set_atlas_eq_def, atlas_eq_is_hol_eq_def]
  \\ metis_tac[]
QED

Theorem atlas_eq_is_hol_eq_imp_set_atlas_eq_eq_eq:
  atlas_eq_is_hol_eq ==> !U V. set_atlas_eq U V <=> (U = V)
Proof
  rw[set_atlas_eq_def]
  \\ simp[atlas_eq_is_hol_eq_imp_mem_set_atlas_eq_eq_IN]
  \\ metis_tac[EXTENSION]
QED

val _ = export_theory ();
