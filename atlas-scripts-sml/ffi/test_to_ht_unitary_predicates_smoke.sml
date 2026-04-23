use "atlas-scripts-sml/to_ht.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(* Smoke test for `ToHT.is_unitary_to_ht` / `is_unitary_to_hts`.
   Semantics: for `HT>=0`, checks purity of the truncated hermitian form. *)

val g = AtlasFFI.atlas_group_new_simple (#"A", 2, #"s", 0);
val p = Representations.trivial g;

val () = if AtlasFFI.atlas_param_is_standard p = 1 andalso AtlasFFI.atlas_param_is_final p = 1 then () else raise Fail "expected standard+final";
val () = if AtlasFFI.atlas_param_is_hermitian p = 1 then () else raise Fail "expected hermitian";

val u0 = ToHT.is_unitary_to_ht (p, 0);
val u1 = ToHT.is_unitary_to_ht (p, 1);
val u2 = ToHT.is_unitary_to_hts (p, []);
val u3 = ToHT.is_unitary_to_hts (p, [0, 1, 2]);

val hf0 = ToHT.hermitian_form_irreducible_to_ht (p, 0);
val rank = ToHT.paramRank p;
val ts = KTypePol.terms (hf0, rank);
val () = if List.all (fn t => #height t <= 0) ts then () else raise Fail "hermitian_form_irreducible_to_ht did not truncate";
val () = if u0 = KTypePol.isPure (hf0, rank) then () else raise Fail "is_unitary_to_ht should match purity of truncated form";
val () = KTypePol.free hf0;

val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";
