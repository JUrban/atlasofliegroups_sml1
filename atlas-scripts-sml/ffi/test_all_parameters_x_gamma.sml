use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/AllParameters.sml";
use "atlas-scripts-sml/Dominant.sml";

structure TestAllParametersXGamma = struct
  fun parseInts s =
    let
      fun toInt tok =
        case Int.fromString tok of
          SOME n => n
        | NONE => raise Fail ("test_all_parameters_x_gamma: bad int token: " ^ tok)
    in
      List.map toInt (String.tokens Char.isSpace s)
    end

  fun parseRatWeightText s : {den: int, nums: int list} =
    (case parseInts s of
       den :: rest => {den = den, nums = rest}
     | _ => raise Fail ("test_all_parameters_x_gamma: bad ratweight text: " ^ s))

  fun run () =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      val ps = AllParameters.all_parameters_x_gamma (g, 4, {den = 1, nums = [0, 1]})
      val pl = AllParameters.all_parameters_x_gamma (g, 3, {den = 1, nums = [1, 0]})
      val () = if length ps <> 2 then raise Fail "test_all_parameters_x_gamma: expected 2 (short)" else ()
      val () = if length pl <> 2 then raise Fail "test_all_parameters_x_gamma: expected 2 (long)" else ()

      val gammaRaw = {den = 1, nums = [~3, 1]}
      val gammaDomText = Dominant.makeDominantText g "1 -3 1"
      val gammaDom = parseRatWeightText gammaDomText
      val ps1 = AllParameters.all_parameters_x_gamma_dominant (g, 4, gammaRaw)
      val ps2 = AllParameters.all_parameters_x_gamma_raw (g, 4, gammaDom)
      val () = if length ps1 = length ps2 then () else raise Fail "test_all_parameters_x_gamma: dominant mismatch"

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
      val () = AllParameters.freeAll ps1
      val () = AllParameters.freeAll ps2
      val () = AtlasFFI.atlas_group_free g
    in
      TextIO.print "ok\n"
    end
end

val () = TestAllParametersXGamma.run ();
