(* Smoke test for `DeformPlus.chamber_rep` and `RootDatum.coxeterNumber`. *)
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/RootDatum.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/deform_plus.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0)
val () = assert "group created" (g <> Foreign.Memory.null)

val rd = AtlasFFI.atlas_group_rootdatum_new g
val () = assert "rootdatum created" (rd <> Foreign.Memory.null)
val h = RootDatum.coxeterNumber rd
val () = RootDatum.free rd
val () = print ("A1 coxeter number = " ^ Int.toString h ^ "\n")
val () = assert "A1 coxeter number is 2" (h = 2)

val p0 = Representations.trivial g
val p = AtlasFFI.atlas_param_normalise p0
val () = AtlasFFI.atlas_param_free p0
val () = assert "param normalised" (p <> Foreign.Memory.null)

val q = DeformPlus.chamber_rep p
val () = print ("chamber_rep nu = " ^ AtlasFFI.atlas_param_nu_text q ^ "\n")

val () = AtlasFFI.atlas_param_free q
val () = AtlasFFI.atlas_param_free p
val () = AtlasFFI.atlas_group_free g

