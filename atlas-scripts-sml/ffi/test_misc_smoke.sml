use "atlas-scripts-sml/misc.sml";

(*
  File: atlas-scripts-sml/ffi/test_misc_smoke.sml

  Purpose
  - Smoke tests for a few `misc.at` compatibility helpers.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val b = Misc.box (2, 3);
val () = assert "box size" (length b = 8);
val () = assert "box first" (hd b = [0, 0, 0]);

val v = Misc.to_binary (5, 6);
val () = assert "to_binary(5,6)=00110" (v = [0, 0, 1, 1, 0]);

val xs = Misc.delete_trailing_zeros [1, 0, 2, 0, 0];
val () = assert "delete_trailing_zeros" (xs = [1, 0, 2]);

val ys = Misc.delete_leading_zeros [0, 0, 3, 0];
val () = assert "delete_leading_zeros" (ys = [3, 0]);

