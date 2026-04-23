use "atlas-scripts-sml/basic.sml";

(*
  File: atlas-scripts-sml/ffi/test_basic_set_helpers_smoke.sml

  Purpose
  - Smoke test for the `list` / `complement` / `complement_of_list` /
    `power_set_int` helpers in `Basic`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val xs = Basic.list (10, fn i => i mod 3 = 0);
val () = assert "list mod3" (xs = [0, 3, 6, 9]);

val ys = Basic.complement (6, fn i => i < 2 orelse i = 4);
val () = assert "complement predicate" (ys = [2, 3, 5]);

val zs = Basic.complement_of_list (5, [1, 3, 7]);
val () = assert "complement_of_list ignores oob" (zs = [0, 2, 4]);

val ps = Basic.power_set_int 3;
val () = assert "power_set_int size" (length ps = 8);
val () = assert "power_set_int contains [0,2]" (List.exists (fn s => s = [0, 2]) ps);

