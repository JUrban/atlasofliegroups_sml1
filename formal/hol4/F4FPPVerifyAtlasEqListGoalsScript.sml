(*
  File: formal/hol4/F4FPPVerifyAtlasEqListGoalsScript.sml

  Purpose
  - Provide small, re-usable list-level definitions/lemmas that talk about
    Atlas's semantic equality `atlas_eq` without assuming it coincides with
    HOL `=`.

  Why this is separate
  - Multiple theories need the notion “membership in a list modulo `atlas_eq`”
    (e.g. ParamHash representation correctness, program-level set equality).
  - Keeping this in a tiny standalone theory avoids dependency cycles between
    the ParamHash state model and bridge/decomposition layers.
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;

open F4FPPVerifySpecTheory;
open F4FPPVerifyAtlasFFIContractsGoalsTheory;

val _ = new_theory "F4FPPVerifyAtlasEqListGoals";

(* Membership in a list modulo Atlas's semantic equality. *)
Definition mem_atlas_eq_def:
  mem_atlas_eq (p:param) (xs:param list) <=> ?q. MEM q xs /\ atlas_eq p q
End

Theorem atlas_eq_is_hol_eq_imp_mem_atlas_eq_eq_MEM:
  atlas_eq_is_hol_eq ==> !p xs. mem_atlas_eq p xs <=> MEM p xs
Proof
  rw[mem_atlas_eq_def, atlas_eq_is_hol_eq_def]
  \\ metis_tac[]
QED

val _ = export_theory ();

