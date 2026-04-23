(*
  File: atlas-scripts-sml/FPPFlags.sml

  Purpose
  - Centralized runtime flags mirroring the global variables used in the `.at`
    scripts (especially the FPP/unitarity verification pipeline).

  Notes
  - These are `ref` cells so that driver scripts can set flags before invoking
    computation/verification routines.
  - Only a subset of the original `.at` flags are currently used by the SML
    port; additional flags can be added as translations expand.
*)
structure FPPFlags = struct
  (* Toggle bottom-layer tests. *)
  val test_bl_flag = ref false
  (* Toggle “revert” logic (script-specific; currently kept for parity). *)
  val revert_flag = ref false
  (* Iterate over every KGB element (vs. selected ones). *)
  val every_KGB_flag = ref false
  (* Include unipotent checks (not used in current F4 verifier). *)
  val unip_flag = ref true
  (* Iterate over every lambda in tables (vs. selected ones). *)
  val every_lambda_flag = ref false
  (* Use legacy projection conventions (kept for script parity). *)
  val old_proj_flag = ref false
  (* Write intermediate x-data / diagnostics (kept for script parity). *)
  val write_x_flag = ref false
  (* When true, enforce Dirac/unitarity-related filters during generation. *)
  val Dirac_flag = ref true
  (* When true, try equal-rank `to_ht`-based early-disproof before exact unitarity checks. *)
  val to_ht_prune_flag = ref false
  (* Number of pruning heights to try (when enabled). *)
  val to_ht_prune_steps = ref 2
  (* Step size for pruning heights (when enabled). *)
  val to_ht_prune_step_size = ref 5
  (* Emit detailed per-lambda diagnostics (script parity). *)
  val every_lambda_deets_flag = ref false
  (* Verbose face/barycenter processing diagnostics. *)
  val face_verbose = ref false
  (* Verbose fundamental-face diagnostics. *)
  val fund_face_verbose = ref false
  (* Apply one-level revert logic (script parity). *)
  val one_level_revert_flag = ref true
  (* General verbose flag for long-running computations. *)
  val final_verbose = ref false
end
