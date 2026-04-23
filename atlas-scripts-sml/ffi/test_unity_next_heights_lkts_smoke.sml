use "atlas-scripts-sml/unity_fpp.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `UnityFPP.next_heights_lkts`. *)

val g = AtlasFFI.atlas_group_new_simple (#"A", 2, #"s", 0);
val p = Representations.trivial g;

val hs = UnityFPP.next_heights_lkts (p, 3);
val () = if length hs <= 3 then () else raise Fail "expected at most 3 heights";
val () = if List.all (fn h => h > 0) hs then () else raise Fail "expected positive heights";

val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";
