use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/ParamHash.sml";
use "atlas-scripts-sml/F4_FPP_points_compute.sml";
use "atlas-scripts-sml/FPPFlags.sml";
use "atlas-scripts-sml/FPP_globalDirac.sml";

(*
  File: atlas-scripts-sml/VerifyF4FPP.sml

  Purpose
  - Self-contained “driver” that reproduces (in SML) the behavior of
    `atlas-scripts/script_to_verify_F4_FPP_unitary_dual.at`:
      - build the F4 FPP parameter hash (expected size 1864)
      - run the bottom-layer / unitary / unitary-dual verification checks

  Usage
  - Typical invocation:
      poly -q < atlas-scripts-sml/script_to_verify_F4_FPP_unitary_dual.sml
    which loads this file and calls `VerifyF4FPP.run()`.
*)
structure VerifyF4FPP = struct
  (* Run the full verification pipeline for `F4_s`. Prints progress and
     consistency checks; raises `Fail` on internal errors. *)
  fun run () : unit =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
      val uhash = ParamHash.create 4096

      val () = FPPFlags.test_bl_flag := false
      val () = FPPFlags.revert_flag := false
      val () = FPPFlags.every_KGB_flag := true
      val () = FPPFlags.unip_flag := false
      val () = FPPFlags.every_lambda_flag := true
      val () = FPPFlags.old_proj_flag := true
      val () = FPPFlags.write_x_flag := true
      val () = FPPFlags.Dirac_flag := true
      val () = FPPFlags.every_lambda_deets_flag := true
      val () = FPPFlags.face_verbose := true
      val () = FPPFlags.fund_face_verbose := true
      val () = FPPFlags.one_level_revert_flag := true
      val () = FPPFlags.final_verbose := true

      val () = F4_FPP_points_compute.computeAllIntoParamHash (g, uhash)

      val () = TextIO.print (Bool.toString (ParamHash.size uhash = 1864) ^ "\n")

      val () = FPP_globalDirac.FPP_unitary_hash_bottom_layer_param_hash (g, uhash)

      val () = TextIO.print (Int.toString (ParamHash.size uhash) ^ "\n")

      val () = ParamHash.freeAll uhash
      val () = AtlasFFI.atlas_group_free g
    in
      ()
    end
end
