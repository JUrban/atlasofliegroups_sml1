use "atlas-scripts-sml/parabolics.sml";

fun assertTrue msg b = if b then () else raise Fail ("assertTrue: " ^ msg);
fun assertInRange msg (lo:int, hi:int) (x:int) =
  assertTrue (msg ^ " out of range: " ^ Int.toString x)
    (lo <= x andalso x <= hi);

val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0);
val n = AtlasFFI.atlas_group_kgb_size g;
val r = AtlasFFI.atlas_group_semisimple_rank g;

val () = assertTrue "expected rank=2 for G2" (r = 2);
val () = assertTrue "expected nonempty KGB" (n > 0);

val x0 = 0;
val S0 = [0];
val S01 = [0, 1];

val mx0 = Parabolics.maximal g (S0, x0);
val () = assertInRange "maximal(S0,x0)" (0, n - 1) mx0;

val mn0 = Parabolics.x_min g (S0, x0);
val () = assertInRange "x_min(S0,x0)" (0, n - 1) mn0;

val mx01 = Parabolics.maximal g (S01, x0);
val () = assertInRange "maximal(S01,x0)" (0, n - 1) mx01;

val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

