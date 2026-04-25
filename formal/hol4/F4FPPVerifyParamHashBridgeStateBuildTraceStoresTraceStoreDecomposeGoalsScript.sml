(*
  File: formal/hol4/F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceStoreDecomposeGoalsScript.sml

  Purpose
  - Decompose the trace-level stores predicate
      `paramhash_build_ps_stores_U_fast_atlas_eq g`
    into two directional obligations that are convenient targets for
    translator/CakeML proofs.

  Summary
  - The trace-level stores predicate is:
      `set_atlas_eq (U_fast g) (set (paramhash_build_ps g))`.
  - A common proof shape is to establish two inclusions (modulo `atlas_eq`):
      (1) every `U_fast` element is represented in the trace, and
      (2) every trace element is represented in `U_fast`.

  Status
  - OK (no `cheat`): purely logical decomposition/recombination lemmas.
*)

open HolKernel Parse boolLib bossLib;

open pred_setTheory pred_setLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;
open F4FPPVerifySpecAtlasEqGoalsTheory;
open F4FPPVerifyGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceGoalsTheory;
open F4FPPVerifyParamHashBridgeStateBuildTraceStoresGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashBridgeStateBuildTraceStoresTraceStoreDecomposeGoals";

(* Direction 1: every `U_fast` element is represented (up to `atlas_eq`) in the trace. *)
Definition paramhash_build_ps_complete_U_fast_atlas_eq_def:
  paramhash_build_ps_complete_U_fast_atlas_eq g <=>
    !p. p IN U_fast g ==> mem_set_atlas_eq p (set (paramhash_build_ps g))
End

(* Direction 2: every trace element is represented (up to `atlas_eq`) in `U_fast`. *)
Definition paramhash_build_ps_sound_U_fast_atlas_eq_def:
  paramhash_build_ps_sound_U_fast_atlas_eq g <=>
    !p. p IN set (paramhash_build_ps g) ==> mem_set_atlas_eq p (U_fast g)
End

(* Alternative, “already-closed” form: these are exactly the two directions of
   `set_atlas_eq (U_fast g) (set trace)` unfolded. These do not require
   assumptions about `atlas_eq`. *)
Definition paramhash_build_ps_complete_U_fast_mod_atlas_eq_def:
  paramhash_build_ps_complete_U_fast_mod_atlas_eq g <=>
    !p. mem_set_atlas_eq p (U_fast g) ==> mem_set_atlas_eq p (set (paramhash_build_ps g))
End

Definition paramhash_build_ps_sound_U_fast_mod_atlas_eq_def:
  paramhash_build_ps_sound_U_fast_mod_atlas_eq g <=>
    !p. mem_set_atlas_eq p (set (paramhash_build_ps g)) ==> mem_set_atlas_eq p (U_fast g)
End

Theorem build_ps_sound_and_complete_imp_build_ps_stores_U_fast_atlas_eq:
  !g.
    atlas_eq_equiv /\
    paramhash_build_ps_complete_U_fast_atlas_eq g /\
    paramhash_build_ps_sound_U_fast_atlas_eq g ==>
      paramhash_build_ps_stores_U_fast_atlas_eq g
Proof
  rpt strip_tac
  \\ fs[paramhash_build_ps_stores_U_fast_atlas_eq_def]
  \\ match_mp_tac sound_and_complete_mod_atlas_eq_gives_set_atlas_eq
  \\ fs[paramhash_build_ps_complete_U_fast_atlas_eq_def,
        paramhash_build_ps_sound_U_fast_atlas_eq_def]
QED

Theorem build_ps_stores_U_fast_atlas_eq_iff_mod_sound_and_complete:
  !g.
    paramhash_build_ps_stores_U_fast_atlas_eq g <=>
      paramhash_build_ps_complete_U_fast_mod_atlas_eq g /\
      paramhash_build_ps_sound_U_fast_mod_atlas_eq g
Proof
  rw[paramhash_build_ps_stores_U_fast_atlas_eq_def, set_atlas_eq_def,
     paramhash_build_ps_complete_U_fast_mod_atlas_eq_def,
     paramhash_build_ps_sound_U_fast_mod_atlas_eq_def]
  \\ EQ_TAC
  >- (
    strip_tac
    \\ conj_tac
    \\ rpt strip_tac
    \\ first_x_assum (qspec_then `p` mp_tac)
    \\ metis_tac[])
  \\ strip_tac
  \\ gen_tac
  \\ EQ_TAC
  \\ metis_tac[]
QED

Theorem build_ps_stores_U_fast_atlas_eq_imp_sound_and_complete:
  !g.
    atlas_eq_equiv /\
    paramhash_build_ps_stores_U_fast_atlas_eq g ==>
      paramhash_build_ps_complete_U_fast_atlas_eq g /\
      paramhash_build_ps_sound_U_fast_atlas_eq g
Proof
  rpt strip_tac
  \\ fs[paramhash_build_ps_complete_U_fast_atlas_eq_def,
        paramhash_build_ps_sound_U_fast_atlas_eq_def]
  \\ rpt strip_tac
  >- (
    (* `U_fast` -> trace *)
    fs[paramhash_build_ps_stores_U_fast_atlas_eq_def, set_atlas_eq_def]
    \\ `mem_set_atlas_eq p (U_fast g)` by
         (fs[mem_set_atlas_eq_def, atlas_eq_equiv_def] \\ metis_tac[])
    \\ metis_tac[])
  \\ (* trace -> `U_fast` *)
  fs[paramhash_build_ps_stores_U_fast_atlas_eq_def, set_atlas_eq_def]
  \\ `mem_set_atlas_eq p (set (paramhash_build_ps g))` by
       (fs[mem_set_atlas_eq_def, atlas_eq_equiv_def] \\ metis_tac[])
  \\ metis_tac[]
QED

val _ = export_theory ();
