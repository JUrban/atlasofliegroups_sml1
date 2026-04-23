use "atlas-scripts-sml/chopping_facets_fast.sml";

(*
  File: atlas-scripts-sml/ffi/test_chopping_facets_fast_smoke.sml

  Purpose
  - Smoke test for `atlas-scripts-sml/chopping_facets_fast.sml`.
*)

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg);

val v0 : Lattice.ratvec = {den = 1, nums = [0]};
val v2 : Lattice.ratvec = {den = 1, nums = [2]};
val vs = [v0, v2];
val f = [1];

val new = Chopping_facets_fast.new_verts (vs, f, 2);
val () = assert "new_verts adds midpoint at 1" (List.exists (fn v => #den v = 1 andalso #nums v = [1]) new);

val regions = Chopping_facets_fast.chop (vs, f, 2);
val () = assert "chop yields nonempty regions" (List.all (fn r => not (null r)) regions andalso not (null regions));

