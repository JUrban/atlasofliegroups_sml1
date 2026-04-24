(*
  File: formal/hol4/F4FPPVerifyAtlasFFIContractsGoalsScript.sml

  Purpose
  - Centralize the “FFI contracts” that we expect to assume/prove about the
    Atlas C++ library calls used by the Poly/ML scripts in `atlas-scripts-sml/`.
  - These contracts are *not* yet connected to any concrete FFI semantics;
    they are a named inventory of assumptions and eventual proof obligations.

  Why this matters
  - The verification plan in `VERIFY_ESTIMATE.md` splits work into:
      (1) pure algorithm/data-structure proofs (CakeML-friendly), and
      (2) semantic correctness of Atlas primitives (FFI / C++ library).
  - Without an explicit contract layer, those assumptions remain implicit and
    tend to leak into unrelated lemmas.

  Relation to existing theories
  - `F4FPPVerifySpecTheory` and `F4FPPBottomLayerGoalsTheory` already introduce
    many Atlas-relevant primitives as *uninterpreted* constants
      (`mk_param`, `first_final_term`, `is_unitary`, `contragredient`, ...).
  - This file adds an explicit “hash/equality” surface (needed for ParamHash)
    and packages all expected properties into named predicates.

  Status
  - Definitions are OK.
  - The theorems are “contracts”: most are left as `cheat`ed placeholders for
    later discharge (or maintained as axioms if we choose an axiomatic FFI).
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifySpecTheory;
open F4FPPBottomLayerGoalsTheory;

val _ = new_theory "F4FPPVerifyAtlasFFIContractsGoals";

(* ------------------------------------------------------------------------- *)
(*  Hash/equality surface (for ParamHash)                                     *)
(* ------------------------------------------------------------------------- *)

(* A semantic equality relation as implemented by Atlas (used by ParamHash). *)
val _ = new_constant ("atlas_eq", ``:param -> param -> bool``);

(* Atlas-provided hash function into `0..m-1` (the C++ API takes `m`). *)
val _ = new_constant ("atlas_hash_mod", ``:param -> num -> num``);

(* Cloning is used by the concrete SML `ParamHash.match` implementation. *)
val _ = new_constant ("atlas_clone", ``:param -> param``);

Definition atlas_eq_equiv_def:
  atlas_eq_equiv <=>
    (!p. atlas_eq p p) /\
    (!p q. atlas_eq p q ==> atlas_eq q p) /\
    (!p q r. atlas_eq p q /\ atlas_eq q r ==> atlas_eq p r)
End

Definition atlas_hash_respects_eq_def:
  atlas_hash_respects_eq <=>
    !m p q. atlas_eq p q ==> atlas_hash_mod p m = atlas_hash_mod q m
End

Definition atlas_hash_range_def:
  atlas_hash_range <=>
    !m p. m <> 0 ==> atlas_hash_mod p m < m
End

Definition atlas_clone_ok_def:
  atlas_clone_ok <=>
    !p. atlas_eq (atlas_clone p) p
End

(* The key bundle needed to justify a bucketed hash-set skeleton. *)
Definition atlas_hash_eq_ok_def:
  atlas_hash_eq_ok <=>
    atlas_eq_equiv /\
    atlas_hash_respects_eq /\
    atlas_hash_range /\
    atlas_clone_ok
End

(* Optional “alignment” contract: treat Atlas equality as the HOL equality on
   the abstract `param` type. If we keep parameters as semantic objects in HOL,
   this is the simplest (and common) modeling choice. *)
Definition atlas_eq_is_hol_eq_def:
  atlas_eq_is_hol_eq <=>
    !p q. atlas_eq p q <=> (p = q)
End

(* ------------------------------------------------------------------------- *)
(*  Congruence / stability contracts for semantic primitives                   *)
(* ------------------------------------------------------------------------- *)

(* Atlas equality should respect the semantic primitives we query in the
   bottom-layer checks. (If `atlas_eq_is_hol_eq` holds, these are immediate.) *)
Definition atlas_eq_congruent_bottom_layer_def:
  atlas_eq_congruent_bottom_layer <=>
    (!p q. atlas_eq p q ==> (is_standard p <=> is_standard q)) /\
    (!p q. atlas_eq p q ==> (is_final p <=> is_final q)) /\
    (!p q. atlas_eq p q ==> (is_hermitian p <=> is_hermitian q)) /\
    (!p q. atlas_eq p q ==> (is_unitary p <=> is_unitary q)) /\
    (!g p q. atlas_eq p q ==> (lambda_table_ok g p <=> lambda_table_ok g q)) /\
    (!p p' q q'. atlas_eq p p' /\ atlas_eq q q' ==> (param_equiv p q <=> param_equiv p' q')) /\
    (!p q. atlas_eq p q ==> atlas_eq (twist p) (twist q)) /\
    (!p q. atlas_eq p q ==> atlas_eq (contragredient p) (contragredient q))
End

(* A minimal algebraic contract for the derived operations. *)
Definition atlas_bottom_layer_algebra_def:
  atlas_bottom_layer_algebra <=>
    (!p. atlas_eq (contragredient (contragredient p)) p)
End

(* Contracts for `first_final_term` (as used in the slow/fast domain witness). *)
Definition atlas_eq_congruent_first_final_def:
  atlas_eq_congruent_first_final <=>
    !p q. atlas_eq p q ==> first_final_term p = first_final_term q
End

(* Bundle: what we expect about Atlas primitives for the F4/FPP effort. *)
Definition atlas_ffi_contracts_def:
  atlas_ffi_contracts <=>
    atlas_hash_eq_ok /\
    atlas_eq_congruent_bottom_layer /\
    atlas_bottom_layer_algebra /\
    atlas_eq_congruent_first_final
End

(* ------------------------------------------------------------------------- *)
(*  Contract theorems (placeholders)                                          *)
(* ------------------------------------------------------------------------- *)

Theorem atlas_eq_is_hol_eq_imp_contracts_simplify:
  atlas_eq_is_hol_eq ==> atlas_eq_congruent_bottom_layer /\ atlas_eq_congruent_first_final
Proof
  rw[atlas_eq_is_hol_eq_def, atlas_eq_congruent_bottom_layer_def, atlas_eq_congruent_first_final_def]
QED

Theorem atlas_hash_eq_ok_expected_for_paramhash:
  atlas_ffi_contracts ==> atlas_hash_eq_ok
Proof
  simp[atlas_ffi_contracts_def]
QED

(* For now we record the intended assumption that the concrete Atlas FFI meets
   these contracts.  Whether this is proved (via a verified FFI model) or kept
   as an axiom is a project decision. *)
Theorem atlas_ffi_contracts_hold:
  atlas_ffi_contracts
Proof
  cheat
QED

val _ = export_theory ();
