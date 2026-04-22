use "atlas-scripts-sml/to_ht.sml";
use "atlas-scripts-sml/representations.sml";

val g = AtlasFFI.atlas_group_new_simple (#"A", 2, #"s", 0);
val nu = {den = 1, nums = [1, 0]};
val p = Representations.minimal_spherical_principal_series (g, nu);

val pol = AtlasFFI.atlas_param_full_deform p;
val () = if pol = Foreign.Memory.null then raise Fail ("full_deform failed: " ^ AtlasFFI.atlas_last_error ()) else ();
val rank = AtlasFFI.atlas_group_rank g;

val n0 = length (KTypePol.terms (pol, rank));

val polAll = ToHT.to_ht (pol, ~1);
val nAll = length (KTypePol.terms (polAll, rank));
val () = if nAll = n0 then () else raise Fail "to_ht(~1) should keep all terms";

val pol0 = ToHT.to_ht (pol, 0);
val ts0 = KTypePol.terms (pol0, rank);
val () = if length ts0 <= n0 then () else raise Fail "to_ht(0) increased term count";
val () = if List.all (fn t => #height t <= 0) ts0 then () else raise Fail "to_ht(0) filter failed";

val () = KTypePol.free pol0;
val () = KTypePol.free polAll;
val () = KTypePol.free pol;
val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;

val () = print "OK\n";

