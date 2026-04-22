use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/AllParameters.sml";

structure TestAllParametersXGamma = struct
  fun run () =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      val ps = AllParameters.all_parameters_x_gamma (g, 4, {den = 1, nums = [0, 1]})
      val pl = AllParameters.all_parameters_x_gamma (g, 3, {den = 1, nums = [1, 0]})
      val () = if length ps <> 2 then raise Fail "test_all_parameters_x_gamma: expected 2 (short)" else ()
      val () = if length pl <> 2 then raise Fail "test_all_parameters_x_gamma: expected 2 (long)" else ()
      val () =
        List.app
          (fn p =>
             if AtlasFFI.atlas_param_is_final p <> 1 then
               raise Fail "test_all_parameters_x_gamma: expected final parameter"
             else
               ())
          (ps @ pl)
      val () = AllParameters.freeAll ps
      val () = AllParameters.freeAll pl
      val () = AtlasFFI.atlas_group_free g
    in
      TextIO.print "ok\n"
    end
end

val () = TestAllParametersXGamma.run ();

