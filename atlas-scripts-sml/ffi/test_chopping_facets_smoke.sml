use "atlas-scripts-sml/chopping_facets.sml";

(*
  File: atlas-scripts-sml/ffi/test_chopping_facets_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/chopping_facets.sml`.
  - Exercises the convex-hull membership helpers and a small `chop` example.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val square : Chopping_facets.ratvec list =
  [ {den = 1, nums = [0, 0]}
  , {den = 1, nums = [2, 0]}
  , {den = 1, nums = [0, 2]}
  , {den = 1, nums = [2, 2]}
  ];

val center : Chopping_facets.ratvec = {den = 1, nums = [1, 1]};

val () = assert "center in hull(square)" (Chopping_facets.in_hull (square, center));
val verts = Chopping_facets.vertices_of_hull (center :: square);
val () = assert "vertices_of_hull drops interior point" (length verts = 4);

val f : Chopping_facets.functional = [1, 0];
val regions = Chopping_facets.chop (square, f);
val () = assert "chop produces 3 regions" (length regions = 3);
val () = assert "chop regions nonempty" (List.all (fn r => not (null r)) regions);

