use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/cross_W_orbit.sml";
use "atlas-scripts-sml/WeylWord.sml";

(*
  File: atlas-scripts-sml/ffi/test_cross_W_orbit_cross_divide_smoke.sml

  Purpose
  - Smoke test for `CrossWOrbit.cross_divide`.

  What it checks
  - For a single generator step `y = cross(s, x)`, `cross_divide(y,x)` returns
    a word that reproduces `y` under the `.at` left cross action.

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_cross_W_orbit_cross_divide_smoke.sml`
*)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val x = 0
val s = 0
val y = AtlasFFI.atlas_kgb_cross (g, s, x)
val () = if y < 0 then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val w = CrossWOrbit.cross_divide (g, y, x)
val y2 = WeylWord.kgbCrossLeft (g, w, x)
val () = if y2 = y then () else raise Fail "cross_divide witness does not reproduce target"

val () = AtlasFFI.atlas_group_free g
val () = TextIO.print "OK\n"

