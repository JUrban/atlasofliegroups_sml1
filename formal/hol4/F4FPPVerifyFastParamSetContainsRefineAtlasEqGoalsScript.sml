(*
  File: formal/hol4/F4FPPVerifyFastParamSetContainsRefineAtlasEqGoalsScript.sml

  Purpose
  - Refine `fast_param_set_contains_ok_atlas_eq` into one-way obligations, in
    direct analogy with `F4FPPVerifyFastParamSetContainsRefineGoalsTheory` but
    with the realistic modulo-`atlas_eq` membership predicate.

  Definitions
  - Soundness:
      `contains p ⇒ mem_set_atlas_eq p (U_fast g)`
  - Completeness:
      `mem_set_atlas_eq p (U_fast g) ⇒ contains p`

  Status
  - “OK”: purely logical decomposition and recombination.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;

open F4FPPVerifyFastParamSetRefineAtlasEqGoalsTheory;

val _ = new_theory "F4FPPVerifyFastParamSetContainsRefineAtlasEqGoals";

Definition fast_param_set_contains_sound_atlas_eq_def:
  fast_param_set_contains_sound_atlas_eq g <=>
    !p. ps_contains (fast_param_set g) p ==> mem_set_atlas_eq p (U_fast g)
End

Definition fast_param_set_contains_complete_atlas_eq_def:
  fast_param_set_contains_complete_atlas_eq g <=>
    !p. mem_set_atlas_eq p (U_fast g) ==> ps_contains (fast_param_set g) p
End

Theorem fast_param_set_contains_sound_and_complete_atlas_eq_imp_contains_ok_atlas_eq:
  !g.
    fast_param_set_contains_sound_atlas_eq g /\
    fast_param_set_contains_complete_atlas_eq g ==>
      fast_param_set_contains_ok_atlas_eq g
Proof
  rw[ fast_param_set_contains_sound_atlas_eq_def
    , fast_param_set_contains_complete_atlas_eq_def
    , fast_param_set_contains_ok_atlas_eq_def
    ]
  \\ eq_tac \\ metis_tac[]
QED

val _ = export_theory ();

