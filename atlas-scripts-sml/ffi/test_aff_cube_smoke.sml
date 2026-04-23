use "atlas-scripts-sml/aff_cube.sml";

(*
  File: atlas-scripts-sml/ffi/test_aff_cube_smoke.sml

  Purpose
  - Smoke test for the core `aff_cube` port.
  - Checks that for A=I and b=0, the unique intersection point in the cube is
    the origin.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val a : Aff_cube.mat = [[1, 0], [0, 1]]; (* 2x2 identity as columns *)
val b : Aff_cube.vec = [0, 0];

val xs = Aff_cube.aff_cube_extrema (a, b);
val () = assert "one extreme point" (length xs = 1);

val (v, s, eps) = hd xs;
val () = assert "fixed coords are both" (s = [0, 1]);
val () = assert "epsilon is 0,0" (eps = [0, 0]);
val () = assert "v is 0,0" (List.all (fn q => BigRat.equal (q, BigRat.zero ())) v);

val () = assert "extrema_short true" (Aff_cube.aff_cube_extrema_short (a, b));
