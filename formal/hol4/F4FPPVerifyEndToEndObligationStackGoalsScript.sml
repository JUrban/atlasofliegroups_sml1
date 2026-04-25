(*
  File: formal/hol4/F4FPPVerifyEndToEndObligationStackGoalsScript.sml

  Purpose
  - Provide a “maximally explicit” top-down theorem that derives the refined
    main consequence (`U_slow = U_fast` + bottom-layer invariants) from a
    detailed stack of obligations that matches the program’s phase structure.

  Why this exists
  - We already have a clean refined main theorem:
      `refined_obligations_imply_equivalence`
    which depends on the compact bundles:
      `fast_semantic_ok` and `bottom_layer_total_ok`.
  - In practice, to verify the *program*, we will prove those bundles by:
      - compute-phase correctness (domain/witness + ParamHash obligations), and
      - bottom-layer checker correctness (param_set interface checks).
  - This theory records that decomposition explicitly, so the remaining proof
    work becomes: discharge the named obligations, not re-derive the structure.

  Status
  - This is “OK composition”: it does not introduce new `cheat`s, but it does
    depend on earlier bridge layers that may themselves be `CHEATED`.
*)

open HolKernel Parse boolLib bossLib;

open F4FPPVerifyAtlasFFIContractsGoalsTheory;
open F4FPPVerifyAtlasEqSetGoalsTheory;
open F4FPPVerifyRefinedMainGoalsTheory;
open F4FPPVerifyRefinedMainAtlasEqGoalsTheory;
open F4FPPVerifySlowRefineAtlasEqGoalsTheory;

open F4FPPVerifyFastComputeBridgeGoalsTheory;
open F4FPPVerifyFastComputeBridgeDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeStateDecomposeGoalsTheory;
open F4FPPVerifyParamHashBridgeDecomposeGoalsTheory;

open F4FPPBottomLayerGoalsTheory;
open F4FPPBottomLayerParamSetGoalsTheory;
open F4FPPVerifyFastParamSetGoalsTheory;
open F4FPPBottomLayerParamSetAtlasEqGoalsTheory;
open F4FPPVerifyFastParamSetAtlasEqGoalsTheory;

val _ = new_theory "F4FPPVerifyEndToEndObligationStackGoals";

(* Detailed end-to-end theorem: make the ParamHash state bundle + contracts and
   the bottom-layer param_set predicate explicit premises. *)
Theorem obligations_stack_imply_equivalence:
  !g dirac.
    (* FFI/hash/equality contracts for the ParamHash correctness story. *)
    atlas_eq_is_hol_eq /\ atlas_hash_range /\

    (* Fast compute-phase obligations: semantic domain/witness part. *)
    fast_compute_domain_ok g /\

    (* Fast compute-phase obligations: ParamHash state-level bundle. *)
    paramhash_obligations_state_factored g /\

    (* Bottom-layer checker obligations, phrased at the param_set interface. *)
    bottom_layer_ok_param_set g dirac (fast_param_set g) /\

    (* Non-compactness to select the “checks” branch of `bottom_layer_total_ok`. *)
    ~group_is_compact g /\

    (* Slow-side refined obligations. *)
    slow_refinement_ok g /\
    slow_ok_components g ==>
      U_slow g (D_slow g) = U_fast g /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  rpt gen_tac
  \\ rpt strip_tac

  (* Derive the fast compute-phase “ParamHash ok” obligation from the state-level bundle. *)
  \\ `fast_compute_paramhash_ok g` by
       (rw[fast_compute_paramhash_ok_def]
        \\ match_mp_tac paramhash_state_factored_imp_paramhash_obligations_factored
        \\ metis_tac[])

  (* Combine semantic + ParamHash obligations into the full compute bundle. *)
  \\ `fast_compute_obligations g` by
       metis_tac[fast_compute_domain_and_paramhash_ok_imp_fast_compute_obligations]

  (* Derive the refined-fast bundle used by the main theorem. *)
  \\ `fast_semantic_ok g` by
       metis_tac[fast_compute_obligations_imp_fast_semantic_ok]

  (* Derive the param_set representation invariant for the bottom layer. *)
  \\ `fast_param_set_ok g` by
       metis_tac[fast_compute_obligations_imp_fast_param_set_ok]

  (* Bottom-layer: param_set predicate + representation invariant gives set-level check. *)
  \\ `bottom_layer_ok g dirac (U_fast g)` by
       metis_tac[fast_param_set_ok_and_bottom_layer_ok_param_set_imp_bottom_layer_ok]

  (* Non-compact case: promote to `bottom_layer_total_ok`. *)
  \\ `bottom_layer_total_ok g dirac (U_fast g)` by
       (rw[bottom_layer_total_ok_def] \\ metis_tac[])

  \\ metis_tac[refined_obligations_imply_equivalence]
QED

(* Variant: state-level ParamHash bundle stated modulo `atlas_eq`.

   This is the next step in removing `atlas_eq_is_hol_eq` from the ParamHash
   part of the story: the only remaining use of `atlas_eq_is_hol_eq` here is to
   convert modulo-`atlas_eq` ParamHash obligations into the plain ones used by
   the rest of the existing goal stack. *)
Theorem obligations_stack_imply_equivalence_paramhash_atlas_eq:
  !g dirac.
    (* Alignment contract (still used by the non-modulo layers of this stack). *)
    atlas_eq_is_hol_eq /\

    (* Hash/equality contracts for the modulo-`atlas_eq` ParamHash bundle. *)
    atlas_hash_eq_ok /\

    (* Fast compute-phase obligations: semantic domain/witness part. *)
    fast_compute_domain_ok g /\

    (* Fast compute-phase obligations: ParamHash state-level bundle, modulo equality. *)
    paramhash_obligations_state_factored_atlas_eq g /\

    (* Bottom-layer checker obligations, phrased at the param_set interface. *)
    bottom_layer_ok_param_set g dirac (fast_param_set g) /\

    (* Non-compactness to select the “checks” branch of `bottom_layer_total_ok`. *)
    ~group_is_compact g /\

    (* Slow-side refined obligations. *)
    slow_refinement_ok g /\
    slow_ok_components g ==>
      U_slow g (D_slow g) = U_fast g /\
      bottom_layer_total_ok g dirac (U_fast g)
Proof
  rpt gen_tac
  \\ rpt strip_tac

  (* Derive the fast compute-phase “ParamHash ok” obligation from the modulo bundle. *)
  \\ `fast_compute_paramhash_ok g` by
       (rw[fast_compute_paramhash_ok_def]
        \\ metis_tac
             [ paramhash_state_factored_atlas_eq_imp_paramhash_obligations_factored_atlas_eq
             , atlas_eq_is_hol_eq_and_paramhash_obligations_factored_atlas_eq_imp_factored
             ])

  (* Combine semantic + ParamHash obligations into the full compute bundle. *)
  \\ `fast_compute_obligations g` by
       metis_tac[fast_compute_domain_and_paramhash_ok_imp_fast_compute_obligations]

  (* Derive the refined-fast bundle used by the main theorem. *)
  \\ `fast_semantic_ok g` by
       metis_tac[fast_compute_obligations_imp_fast_semantic_ok]

  (* Derive the param_set representation invariant for the bottom layer. *)
  \\ `fast_param_set_ok g` by
       metis_tac[fast_compute_obligations_imp_fast_param_set_ok]

  (* Bottom-layer: param_set predicate + representation invariant gives set-level check. *)
  \\ `bottom_layer_ok g dirac (U_fast g)` by
       metis_tac[fast_param_set_ok_and_bottom_layer_ok_param_set_imp_bottom_layer_ok]

  (* Non-compact case: promote to `bottom_layer_total_ok`. *)
  \\ `bottom_layer_total_ok g dirac (U_fast g)` by
       (rw[bottom_layer_total_ok_def] \\ metis_tac[])

  \\ metis_tac[refined_obligations_imply_equivalence]
QED

(* Full modulo-`atlas_eq` obligation stack:

   This is the version we ultimately want to hook the program-success bridges to.
   It avoids `atlas_eq_is_hol_eq` entirely:
   - the compute phase is phrased using `fast_compute_obligations_atlas_eq`,
     derived from the state-level ParamHash bundle plus `atlas_hash_eq_ok`;
   - the bottom-layer checker is phrased via the param_set interface using
     `bottom_layer_total_ok_param_set_atlas_eq`, and then lifted to
     `bottom_layer_total_ok_atlas_eq`.

   The conclusion matches `F4FPPVerifyRefinedMainAtlasEqGoalsTheory`:
     `set_atlas_eq (U_fast g) (U_slow g (D_slow g))`
   plus the bottom-layer postcondition modulo `atlas_eq`. *)
Theorem obligations_stack_imply_set_atlas_eq_fast_atlas_eq_and_bottom_layer_total_ok_atlas_eq:
  !g dirac.
    atlas_hash_eq_ok /\
    atlas_eq_congruent_bottom_layer /\
    fast_compute_domain_ok_atlas_eq g /\
    paramhash_obligations_state_factored g /\
    bottom_layer_total_ok_param_set_atlas_eq g dirac (fast_param_set g) /\
    slow_refinement_ok g /\
    slow_ok_components_atlas_eq g ==>
      set_atlas_eq (U_fast g) (U_slow g (D_slow g)) /\
      bottom_layer_total_ok_atlas_eq g dirac (U_fast g)
Proof
  rpt gen_tac
  \\ rpt strip_tac
  \\ `atlas_eq_equiv` by fs[atlas_hash_eq_ok_def]

  \\ `fast_compute_obligations_atlas_eq g` by
       metis_tac[fast_compute_domain_and_state_factored_imp_fast_compute_obligations_atlas_eq]
  \\ `fast_semantic_ok_atlas_eq g` by
       metis_tac[fast_compute_obligations_atlas_eq_imp_fast_semantic_ok_atlas_eq]
  \\ `fast_param_set_ok_atlas_eq g` by
       metis_tac[fast_compute_obligations_atlas_eq_imp_fast_param_set_ok_atlas_eq]

  \\ `bottom_layer_total_ok_atlas_eq g dirac (U_fast g)` by
       metis_tac[fast_param_set_ok_atlas_eq_and_bottom_layer_total_ok_param_set_atlas_eq_imp_bottom_layer_total_ok_atlas_eq]

  \\ metis_tac[refined_obligations_imply_set_atlas_eq_fast_atlas_eq_and_bottom_layer_total_ok_atlas_eq]
QED

val _ = export_theory ();
