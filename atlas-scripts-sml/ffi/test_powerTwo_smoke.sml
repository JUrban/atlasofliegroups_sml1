use "atlas-scripts-sml/powerTwo.sml";

(*
  File: atlas-scripts-sml/ffi/test_powerTwo_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/powerTwo.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val () = assert "to_binary 0" (PowerTwo.to_binary 0 = []);
val () = assert "to_binary 1" (PowerTwo.to_binary 1 = [1]);
val () = assert "to_binary 2" (PowerTwo.to_binary 2 = [0, 1]);
val () = assert "to_binary 5" (PowerTwo.to_binary 5 = [1, 0, 1]);

val pf = PowerTwo.make_power_function 3;
val () = assert "power 0" (#power pf 0 = 1);
val () = assert "power 1" (#power pf 1 = 3);
val () = assert "power 2" (#power pf 2 = 9);
val () = assert "power 5" (#power pf 5 = 243);

val () = #clear pf ();
val () = assert "power after clear" (#power pf 4 = 81);

