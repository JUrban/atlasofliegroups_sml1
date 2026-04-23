use "atlas-scripts-sml/FPP_faces_herm.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/representations.sml";

(*
  Smoke test for `FPP_faces_herm.next_height` / `next_heights`.
*)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0);
val p = Representations.trivial g;

val h = FPP_faces_herm.next_height p;
val _ = if h > 0 then () else raise Fail ("expected positive next_height, got " ^ Int.toString h);

val hs = FPP_faces_herm.next_heights (p, 3);
val _ = if length hs <= 3 then () else raise Fail "expected <=3 heights";
val _ = if List.all (fn x => x > 0) hs then () else raise Fail "expected positive heights";

val _ = AtlasFFI.atlas_param_free p;
val _ = AtlasFFI.atlas_group_free g;

val _ = print "OK\n";

