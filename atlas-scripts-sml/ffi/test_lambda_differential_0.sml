use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/LambdaDifferential0.sml";

structure TestLambdaDifferential0 = struct
  fun isPowerOfTwo n =
    n > 0 andalso
    let
      fun loop m =
        m = 1 orelse (m mod 2 = 0 andalso loop (m div 2))
    in
      loop n
    end

  fun run () =
    let
      val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
      val rank = AtlasFFI.atlas_group_rank g
      val kgbSize = AtlasFFI.atlas_group_kgb_size g
      val x = kgbSize - 1
      val vs = LambdaDifferential0.all (g, x)
      val () = if null vs then raise Fail "test_lambda_differential_0: empty" else ()
      val () = if not (isPowerOfTwo (length vs)) then raise Fail "test_lambda_differential_0: count not power of two" else ()
      val () =
        List.app
          (fn v => if length v <> rank then raise Fail "test_lambda_differential_0: bad vector length" else ())
          vs
      val zero = List.tabulate (rank, fn _ => 0)
      val () =
        if List.exists (fn v => v = zero) vs then
          ()
        else
          raise Fail "test_lambda_differential_0: missing zero vector"
      val () = AtlasFFI.atlas_group_free g
    in
      TextIO.print "ok\n"
    end
end

val () = TestLambdaDifferential0.run ();
