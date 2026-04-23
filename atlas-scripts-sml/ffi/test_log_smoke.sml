use "atlas-scripts-sml/log.sml";

(*
  File: atlas-scripts-sml/ffi/test_log_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/log.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val () = assert "max_power_int(1,2)=0" (Log.max_power_int (1, 2) = 0);
val () = assert "max_power_int(2,2)=1" (Log.max_power_int (2, 2) = 1);
val () = assert "max_power_int(3,2)=1" (Log.max_power_int (3, 2) = 1);
val () = assert "max_power_int(4,2)=2" (Log.max_power_int (4, 2) = 2);

val r = Rat.make (3, 2); (* 1.5 *)
val () = assert "max_power_rat(1.5,2)=0" (Log.max_power_rat (r, 2) = 0);

val r2 = Rat.make (9, 2); (* 4.5, square = 20.25, floor=20, max_power base2=4 *)
val () = assert "rounded_log_2(4.5)=2" (Log.rounded_log_2 r2 = 2);

