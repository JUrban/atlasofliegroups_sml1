use "atlas-scripts-sml/to_ht.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `ToHT.is_unitary_to_ht` / `is_unitary_to_hts`.
   Current semantics: exact unitarity check (height bounds ignored). *)

val g = AtlasFFI.atlas_group_new_simple (#"A", 2, #"s", 0);
val p = Representations.trivial g;

val () = if AtlasFFI.atlas_param_is_standard p = 1 andalso AtlasFFI.atlas_param_is_final p = 1 then () else raise Fail "expected standard+final";
val () = if AtlasFFI.atlas_param_is_hermitian p = 1 then () else raise Fail "expected hermitian";

val exact = (AtlasFFI.atlas_param_is_hermitian p = 1 andalso AtlasFFI.atlas_param_is_unitary p = 1);
val u0 = ToHT.is_unitary_to_ht (p, 0);
val u1 = ToHT.is_unitary_to_ht (p, 1);
val u2 = ToHT.is_unitary_to_hts (p, []);
val u3 = ToHT.is_unitary_to_hts (p, [0, 1, 2]);

val () = if u0 = exact andalso u1 = exact andalso u2 = exact andalso u3 = exact then () else raise Fail "unexpected unitary predicate result";

val hf0 = ToHT.hermitian_form_irreducible_to_ht (p, 0);
val rank = ToHT.paramRank p;
val ts = KTypePol.terms (hf0, rank);
val () = if List.all (fn t => #height t <= 0) ts then () else raise Fail "hermitian_form_irreducible_to_ht did not truncate";
val () = KTypePol.free hf0;

val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";
