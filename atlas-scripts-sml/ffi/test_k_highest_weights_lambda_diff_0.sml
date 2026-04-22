use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/K_highest_weights.sml";
use "atlas-scripts-sml/LambdaDifferential0.sml";

structure TestKHighestWeightsLambdaDiff0 = struct
  fun sameSet (xs: int list list, ys: int list list) : bool =
    length xs = length ys
    andalso List.all (fn x => List.exists (fn y => x = y) ys) xs

  fun run () =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      fun check x =
        let
          val viaTheta = K_highest_weights.all_lambda_differential_0 (g, x)
          val viaFFI = LambdaDifferential0.all (g, x)
        in
          if sameSet (viaTheta, viaFFI) then () else raise Fail ("test_k_highest_weights: mismatch at x=" ^ Int.toString x)
        end
      val () = check 3
      val () = check 4
      val () = AtlasFFI.atlas_group_free g
    in
      TextIO.print "ok\n"
    end
end

val () = TestKHighestWeightsLambdaDiff0.run ();

