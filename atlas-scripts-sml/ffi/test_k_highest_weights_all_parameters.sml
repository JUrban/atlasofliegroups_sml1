use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/K_highest_weights.sml";
use "atlas-scripts-sml/G2_unitary_dual.sml";

structure TestKHighestWeightsAllParameters = struct
  fun run () =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      val ps = G2_unitary_dual.p_s g ({num = 2, den = 1}, {num = 0, den = 1})
      val p = hd ps
      val alls = K_highest_weights.all_parameters (g, p)
      val twists = K_highest_weights.all_lambda_differential_0 (g, AtlasFFI.atlas_param_x p)

      val () = if length alls = length twists then () else raise Fail "test_k_highest_weights_all_parameters: length mismatch"
      val nu0 = AtlasFFI.atlas_param_nu_text p
      val () =
        List.app
          (fn q =>
             if AtlasFFI.atlas_param_nu_text q = nu0 then () else raise Fail "test_k_highest_weights_all_parameters: nu mismatch")
          alls

      val () = List.app AtlasFFI.atlas_param_free alls
      val () = List.app AtlasFFI.atlas_param_free ps
      val () = AtlasFFI.atlas_group_free g
    in
      TextIO.print "ok\n"
    end
end

val () = TestKHighestWeightsAllParameters.run ();

