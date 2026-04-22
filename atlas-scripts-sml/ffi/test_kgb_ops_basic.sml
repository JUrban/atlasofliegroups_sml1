use "atlas-scripts-sml/Coordinates.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

fun assertTrue msg b = if b then () else raise Fail ("assertTrue: " ^ msg);
fun assertInRange msg (lo:int, hi:int) (x:int) =
  assertTrue (msg ^ " out of range: " ^ Int.toString x)
    (lo <= x andalso x <= hi);

val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0);
val n = AtlasFFI.atlas_group_kgb_size g;
val rank = AtlasFFI.atlas_group_semisimple_rank g;

val () = assertTrue "expected nonempty KGB" (n > 0);
val () = assertTrue "expected positive rank" (rank > 0);

fun loop_x x =
  if x >= n then ()
  else
    let
      val len = AtlasFFI.atlas_kgb_length (g, x)
      val () = assertTrue ("length negative at x=" ^ Int.toString x) (len >= 0)
      val tfText = AtlasFFI.atlas_kgb_torus_factor_text (g, x)
      val tf = Coordinates.parseRatWeightText tfText
      val () = assertTrue "torus_factor denom 0" (#den tf <> 0)

      fun loop_s s =
        if s >= rank then ()
        else
          let
            val st = AtlasFFI.atlas_kgb_status (g, s, x)
            val () = assertInRange ("status at (s=" ^ Int.toString s ^ ",x=" ^ Int.toString x ^ ")") (0, 4) st
            val y = AtlasFFI.atlas_kgb_cross (g, s, x)
            val () = assertInRange ("cross at (s=" ^ Int.toString s ^ ",x=" ^ Int.toString x ^ ")") (0, n - 1) y
            val z = AtlasFFI.atlas_kgb_cayley (g, s, x)
            val () = assertInRange ("cayley at (s=" ^ Int.toString s ^ ",x=" ^ Int.toString x ^ ")") (0, n - 1) z
          in
            loop_s (s + 1)
          end
    in
      loop_s 0;
      loop_x (x + 1)
    end;

val () = loop_x 0;
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";
