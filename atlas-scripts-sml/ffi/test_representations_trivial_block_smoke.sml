use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/representations.sml";

(*
  File: atlas-scripts-sml/ffi/test_representations_trivial_block_smoke.sml

  Purpose
  - Smoke test for `Representations.trivial_block` and `Representations.block_of`.

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_representations_trivial_block_smoke.sml`
*)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val p = Representations.trivial g
val ps = Representations.block_of p
val () = if null ps then raise Fail "block_of(trivial) returned empty" else ()
val () = if List.exists (fn q => AtlasFFI.atlas_param_equal (p, q) = 1) ps then () else raise Fail "trivial not found in its block"

val () = List.app AtlasFFI.atlas_param_free ps

val ts = Representations.trivial_block g
val () = if null ts then raise Fail "trivial_block returned empty" else ()
val () = List.app AtlasFFI.atlas_param_free ts

val () = AtlasFFI.atlas_param_free p
val () = AtlasFFI.atlas_group_free g

val () = TextIO.print "OK\n"

