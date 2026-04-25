(*
  File: formal/hol4/F4FPPVerifyParamHashStateBuildSetGoalsScript.sml

  Purpose
  - Strengthen the pure ParamHash build model with “set-of-elements” facts that
    are natural translator/CakeML proof targets.

  Main result
  - Building a ParamHash state by repeated `ph_match_state` from the canonical
    empty table (`ph_create_state m`) stores exactly the input list, modulo the
    Atlas semantic equality `atlas_eq`:

      `set_atlas_eq (set ps) (ph_set (ph_build_from_create_state m ps))`.

  Why this matters
  - In the main HOL4 stack we increasingly phrase “stores-U-fast” obligations in
    terms of the canonical build-state model’s `ph_set` view.
  - This file provides the pure, non-cheated lemma that lets us replace “prove a
    property of `ph_set (build_state)`” by the simpler “prove a property of the
    build trace list”, i.e. about `set ps`.

  Status
  - OK (no `cheat`).
*)

open HolKernel Parse boolLib bossLib;

open listTheory listLib;
open pred_setTheory pred_setLib;
open optionTheory;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqListGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;
open F4FPPVerifyParamHashStateGoalsTheory;
open F4FPPVerifyParamHashStateBuildGoalsTheory;

val _ = new_theory "F4FPPVerifyParamHashStateBuildSetGoals";

Theorem find_in_bucket_SOME_imp_MEM_atlas_eq:
  !p b idx.
    find_in_bucket p b = SOME idx ==>
      ?q. MEM (q,idx) b /\ atlas_eq p q
Proof
  Induct_on `b`
  \\ rw[find_in_bucket_def]
  \\ PairCases_on `h`
  \\ fs[find_in_bucket_def]
  \\ Cases_on `atlas_eq p h0`
  \\ fs[]
  \\ metis_tac[]
QED

Theorem ph_lookup_state_SOME_imp_mem_atlas_eq_elems:
  !p s idx.
    atlas_hash_range /\ ph_invariant s /\ ph_lookup_state p s = SOME idx ==>
      mem_atlas_eq p s.elems
Proof
  metis_tac[ph_contains_state_imp_mem_atlas_eq_elems, ph_contains_state_def]
QED

Theorem mem_atlas_eq_elems_imp_mem_atlas_eq_match_elems:
  !p q s.
    mem_atlas_eq p s.elems ==>
      mem_atlas_eq p ( (SND (ph_match_state q s)).elems )
Proof
  rpt strip_tac
  \\ Cases_on `ph_lookup_state q s`
  \\ simp[ph_match_state_def, LET_THM]
  \\ fs[mem_atlas_eq_def, MEM_APPEND]
  \\ metis_tac[]
QED

Theorem ph_match_state_mem_atlas_eq_self:
  !p s.
    atlas_hash_range /\ atlas_eq_equiv /\ ph_invariant s ==>
      mem_atlas_eq p ( (SND (ph_match_state p s)).elems )
Proof
  rpt strip_tac
  \\ Cases_on `ph_lookup_state p s`
  >- (
    (* insert: append `p` itself *)
    simp[ph_match_state_def, LET_THM, mem_atlas_eq_def, MEM_APPEND]
    \\ fs[atlas_eq_equiv_def]
    \\ metis_tac[])
  \\ (* match: table unchanged, but lookup implies an atlas_eq-representative exists *)
  simp[ph_match_state_def, LET_THM]
  \\ match_mp_tac ph_lookup_state_SOME_imp_mem_atlas_eq_elems
  \\ metis_tac[]
QED

Theorem mem_atlas_eq_elems_imp_mem_atlas_eq_build_elems:
  !ps p s.
    mem_atlas_eq p s.elems ==>
      mem_atlas_eq p ( (ph_build_state ps s).elems )
Proof
  Induct_on `ps`
  \\ rw[ph_build_state_def]
  \\ first_x_assum match_mp_tac
  \\ match_mp_tac mem_atlas_eq_elems_imp_mem_atlas_eq_match_elems
  \\ simp[]
QED

Theorem MEM_build_state_imp_mem_atlas_eq_elems:
  !ps p s.
    atlas_hash_range /\ atlas_eq_equiv /\ ph_invariant s /\ MEM p ps ==>
      mem_atlas_eq p ( (ph_build_state ps s).elems )
Proof
  Induct_on `ps`
  >- simp[ph_build_state_def]
  \\ rpt gen_tac
  \\ rpt strip_tac
  \\ rename1 `h::ps`
  \\ fs[ph_build_state_def]
  \\ Cases_on `p = h`
  >- (
    (* Head case. *)
    fs[]
    \\ match_mp_tac mem_atlas_eq_elems_imp_mem_atlas_eq_build_elems
    \\ match_mp_tac ph_match_state_mem_atlas_eq_self
    \\ rpt conj_tac
    \\ fs[])
  \\ (* Tail case: `MEM p ps`. Preserve invariant and appeal to IH. *)
  `MEM p ps` by fs[MEM]
  \\ `ph_invariant (SND (ph_match_state h s))` by
       metis_tac[ph_match_state_preserves_invariant]
  \\ metis_tac[]
QED

Theorem MEM_elems_match_state_imp_MEM_self_or_old:
  !p s q.
    MEM q ( (SND (ph_match_state p s)).elems ) ==> (q = p \/ MEM q s.elems)
Proof
  rpt strip_tac
  \\ Cases_on `ph_lookup_state p s`
  >- (
    fs[ph_match_state_def, LET_THM, MEM_APPEND])
  \\ fs[ph_match_state_def, LET_THM]
QED

Theorem MEM_elems_build_state_imp_MEM_inputs_or_old:
  !ps s q.
    MEM q ( (ph_build_state ps s).elems ) ==> MEM q ps \/ MEM q s.elems
Proof
  Induct_on `ps`
  \\ rw[ph_build_state_def]
  \\ qabbrev_tac `s' = SND (ph_match_state h s)`
  \\ `MEM q ps \/ MEM q s'.elems` by
       (first_x_assum (qspecl_then [`s'`,`q`] mp_tac)
        \\ simp[Abbr`s'`])
  \\ Cases_on `MEM q ps`
  >- (disj1_tac \\ simp[])
  \\ fs[]
  \\ `q = h \/ MEM q s.elems` by
       (qpat_x_assum `MEM q s'.elems` mp_tac
        \\ simp[Abbr`s'`]
        \\ metis_tac[MEM_elems_match_state_imp_MEM_self_or_old])
  \\ Cases_on `q = h`
  >- (disj1_tac \\ simp[])
  \\ disj2_tac
  \\ fs[]
QED

Theorem MEM_elems_build_from_create_imp_MEM_ps:
  !m ps q.
    MEM q ( (ph_build_from_create_state m ps).elems ) ==> MEM q ps
Proof
  rpt strip_tac
  \\ `MEM q ( (ph_build_state ps (ph_create_state m)).elems )` by
       fs[ph_build_from_create_state_def]
  \\ `MEM q ps \/ MEM q (ph_create_state m).elems` by
       metis_tac[MEM_elems_build_state_imp_MEM_inputs_or_old]
  \\ fs[ph_create_state_def]
QED

Theorem ph_build_from_create_state_set_atlas_eq_set_ps:
  !m ps.
    atlas_hash_range /\ atlas_eq_equiv /\ m <> 0 ==>
      set_atlas_eq (set ps) (ph_set (ph_build_from_create_state m ps))
Proof
  rpt strip_tac
  \\ rw[set_atlas_eq_def, ph_set_def]
  \\ simp[GSYM mem_atlas_eq_iff_mem_set_atlas_eq_set]
  \\ eq_tac
  >- (
    rw[mem_atlas_eq_def]
    \\ rename1 `MEM q ps`
    \\ `ph_invariant (ph_create_state m)` by
         metis_tac[ph_create_state_invariant]
    \\ `mem_atlas_eq q ((ph_build_from_create_state m ps).elems)` by
         (simp[ph_build_from_create_state_def]
          \\ match_mp_tac MEM_build_state_imp_mem_atlas_eq_elems
          \\ rpt conj_tac
          >- fs[]
          >- fs[]
          >- fs[]
          \\ fs[])
    \\ fs[mem_atlas_eq_def]
    \\ qexists_tac `q'`
    \\ simp[]
    \\ fs[atlas_eq_equiv_def]
    \\ metis_tac[])
  \\ rw[mem_atlas_eq_def]
  \\ rename1 `MEM q ((ph_build_from_create_state m ps).elems)`
  \\ `MEM q ps` by metis_tac[MEM_elems_build_from_create_imp_MEM_ps]
  \\ metis_tac[mem_atlas_eq_def]
QED

val _ = export_theory ();
