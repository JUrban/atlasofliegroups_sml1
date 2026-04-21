use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/BigUnitaryHash.sml";
use "atlas-scripts-sml/F4_FPP_points.sml";
use "atlas-scripts-sml/FPPFlags.sml";
use "atlas-scripts-sml/FPP_globalDirac.sml";

structure VerifyF4FPP = struct
  fun run () : unit =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
      val big_unitary_hash = BigUnitaryHash.create 4096

      val () = F4_FPP_points.loadInto (g, big_unitary_hash)

      val () = TextIO.print (Bool.toString (BigUnitaryHash.size big_unitary_hash = 1864) ^ "\n")

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

      val () = FPP_globalDirac.FPP_unitary_hash_bottom_layer (g, big_unitary_hash)

      val () = TextIO.print (Int.toString (BigUnitaryHash.size big_unitary_hash) ^ "\n")

      val () = BigUnitaryHash.freeAll big_unitary_hash
      val () = AtlasFFI.atlas_group_free g
    in
      ()
    end
end

