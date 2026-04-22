use "atlas-scripts-sml/unity.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `Unity.full_deform_off_hts` / `next_heights`. *)

val g = AtlasFFI.atlas_group_new_simple (#"A", 2, #"s", 0);
val nu = {den = 1, nums = [1, 0]};
val p = Representations.minimal_spherical_principal_series (g, nu);

val hs = Unity.full_deform_off_hts p;
val () = if List.all (fn h => h >= 0) hs then () else raise Fail "expected nonnegative heights";

val hs2 = Unity.next_heights (p, 2);
val () = if length hs2 <= 2 then () else raise Fail "expected at most 2 heights";
val () = if List.take (hs, length hs2) = hs2 then () else raise Fail "next_heights should take prefix of off_hts";

val () = if Unity.is_unitary_test p = (AtlasFFI.atlas_param_is_unitary p = 1 andalso AtlasFFI.atlas_param_is_hermitian p = 1) then () else raise Fail "is_unitary_test mismatch";

val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";
