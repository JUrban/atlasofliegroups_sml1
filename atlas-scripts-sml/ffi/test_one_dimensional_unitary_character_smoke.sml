use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/one_dimensional.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/ffi/test_one_dimensional_unitary_character_smoke.sml

  Purpose
  - Smoke tests for `OneDimensional.is_one_dimensional` / `is_unitary_character`
    and `unitary_one_dimensional_default`.

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_one_dimensional_unitary_character_smoke.sml`
*)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val p = Representations.trivial g
val () = if OneDimensional.is_one_dimensional (g, p) then () else raise Fail "trivial should be one-dimensional"
val () = if OneDimensional.is_unitary_character (g, p) then () else raise Fail "trivial should be a unitary character"

val chars = OneDimensional.unitary_one_dimensional_default g
val () = if List.exists (fn q => AtlasFFI.atlas_param_equal (p, q) = 1) chars then () else raise Fail "trivial not found in unitary_one_dimensional_default"

val () = List.app AtlasFFI.atlas_param_free chars
val () = AtlasFFI.atlas_param_free p
val () = AtlasFFI.atlas_group_free g

val () = TextIO.print "OK\n"

