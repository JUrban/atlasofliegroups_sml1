use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/AllParameters.sml";

structure TestAllParametersGamma = struct
  fun run () =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      val ps = AllParameters.all_parameters_gamma_dominant (g, {den = 1, nums = [~3, 1]})
      val () = if null ps then raise Fail "test_all_parameters_gamma: expected non-empty" else ()
      val () =
        List.app
          (fn p =>
             if AtlasFFI.atlas_param_is_final p <> 1 then
               raise Fail "test_all_parameters_gamma: expected final parameter"
             else
               ())
          ps
      val () = AllParameters.freeAll ps
      val () = AtlasFFI.atlas_group_free g
    in
      TextIO.print "ok\n"
    end
end

val () = TestAllParametersGamma.run ();

