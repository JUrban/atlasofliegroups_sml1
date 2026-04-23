(* Smoke test for `atlas-scripts-sml/deform_plus.sml`. *)
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/deform_plus.sml";
use "atlas-scripts-sml/KTypePol.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0)
val () = assert "group created" (g <> Foreign.Memory.null)

val p0 = Representations.trivial g
val p = AtlasFFI.atlas_param_normalise p0
val () = AtlasFFI.atlas_param_free p0
val () = assert "param normalised" (p <> Foreign.Memory.null)

val (computed, pol) = DeformPlus.recursive_deform_plus p

val () = print ("computed params: " ^ Int.toString (length computed) ^ "\n")
val () =
  print
    ("KTypePol purity counts (A1 rank=1): " ^ KTypePol.purityString (pol, 1) ^ "\n")

val () = List.app AtlasFFI.atlas_param_free computed
val () = AtlasFFI.atlas_ktypepol_free pol
val () = AtlasFFI.atlas_param_free p
val () = AtlasFFI.atlas_group_free g

