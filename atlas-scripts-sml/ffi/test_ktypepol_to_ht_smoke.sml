use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `atlas_ktypepol_to_ht`:
   - build a parameter with nontrivial `full_deform`
   - truncate to a small height
   - check all remaining terms have height <= cutoff and term count doesn't increase. *)

val g = AtlasFFI.atlas_group_new_simple (#"A", 2, #"s", 0);
val nu = {den = 1, nums = [1, 0]}; (* a simple nonzero choice; should give a nontrivial deformation *)
val p = Representations.minimal_spherical_principal_series (g, nu);

val pol = AtlasFFI.atlas_param_full_deform p;
val () = if pol = Foreign.Memory.null then raise Fail ("full_deform failed: " ^ AtlasFFI.atlas_last_error ()) else ();

val rank = AtlasFFI.atlas_group_rank g;
val ts = KTypePol.terms (pol, rank);
val n0 = length ts;
val () = if n0 > 0 then () else raise Fail "expected nonempty deformation";

val cutoff = 0;
val pol2 = KTypePol.toHT (pol, cutoff);
val ts2 = KTypePol.terms (pol2, rank);
val n1 = length ts2;
val () = if n1 <= n0 then () else raise Fail "toHT increased term count";
val () = if List.all (fn t => #height t <= cutoff) ts2 then () else raise Fail "toHT height filter failed";

val () = KTypePol.free pol2;
val () = KTypePol.free pol;
val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;

val () = print "OK\n";

