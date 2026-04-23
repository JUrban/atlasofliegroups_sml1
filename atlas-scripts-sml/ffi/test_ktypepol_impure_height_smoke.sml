use "atlas-scripts-sml/to_ht.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/KTypePol.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

(*
  Smoke test for `KTypePol.impureHeight` and `ToHT.is_unitary_to_ht_prune_equal_rank_depth`.
*)

val g = AtlasFFI.atlas_group_new_simple (#"A", 2, #"s", 0);
val () = if g = Foreign.Memory.null then raise Fail ("group_new_simple failed: " ^ AtlasFFI.atlas_last_error ()) else ();

val p = Representations.trivial g;
val () = if AtlasFFI.atlas_param_is_hermitian p = 1 then () else raise Fail "expected hermitian";

val hf = ToHT.hermitian_form_irreducible_to_ht (p, 0);
val rank = ToHT.paramRank p;
val d = KTypePol.impureHeight (hf, rank);
val () = if d = ~1 orelse d >= 0 then () else raise Fail "impureHeight should be ~1 or >=0";
val () = KTypePol.free hf;

val d2 = ToHT.is_unitary_to_ht_prune_equal_rank_depth (g, p, 0);
val () = if d2 = ~1 orelse d2 >= 0 then () else raise Fail "depth should be ~1 or >=0 for hermitian params";

val () = AtlasFFI.atlas_param_free p;
val () = AtlasFFI.atlas_group_free g;
val () = print "OK\n";

