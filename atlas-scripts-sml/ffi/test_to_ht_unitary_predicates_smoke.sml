use "atlas-scripts-sml/to_ht.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `ToHT.is_unitary_to_ht` / `is_unitary_to_hts`.
   Current semantics: exact unitarity check (height bounds ignored). *)

val g = AtlasFFI.atlas_group_new_simple (#"A", 2, #"s", 0);
val nu = {den = 1, nums = [0, 0]};
val p = Representations.minimal_spherical_principal_series (g, nu);

val u0 = (AtlasFFI.atlas_param_is_hermitian p = 1 andalso AtlasFFI.atlas_param_is_unitary p = 1);
val u1 = ToHT.is_unitary_to_ht (p, 0);
val u2 = ToHT.is_unitary_to_ht (p, ~1);
val u3 = ToHT.is_unitary_to_hts (p, []);
val u4 = ToHT.is_unitary_to_hts (p, [0, 1, 2]);

val () = if u1 = u0 andalso u2 = u0 andalso u3 = u0 andalso u4 = u0 then () else raise Fail "unitary predicate mismatch";

val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

