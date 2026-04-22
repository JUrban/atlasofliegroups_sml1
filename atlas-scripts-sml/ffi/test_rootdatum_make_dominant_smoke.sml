use "atlas-scripts-sml/finite_dimensional.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/ffi/test_rootdatum_make_dominant_smoke.sml

  Purpose
  - Smoke test for `atlas_rootdatum_make_dominant_ratweight_text` via
    `FiniteDimensional.make_dominant`.

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_rootdatum_make_dominant_smoke.sml`
*)

fun expect (b: bool, msg: string) = if b then () else raise Fail msg

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val rd = AtlasFFI.atlas_group_rootdatum_new g
val () = if rd = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val v = Lattice.ratvecNormalize {den = 1, nums = [~3]}
val vDom = FiniteDimensional.make_dominant (rd, v)
val () = expect (#nums vDom = [3], "A1 make_dominant(~3) expected 3")

val () = AtlasFFI.atlas_rootdatum_free rd
val () = AtlasFFI.atlas_group_free g

val () = TextIO.print "OK\n"

