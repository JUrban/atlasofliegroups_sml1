use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/cross_W_orbit.sml";
use "atlas-scripts-sml/WeylWord.sml";

(*
  File: atlas-scripts-sml/ffi/test_cross_W_orbit_is_in_cross_orbit_smoke.sml

  Purpose
  - Smoke test for `CrossWOrbit.is_in_cross_orbit`.

  What it checks
  - For `y = cross(s,x)` a single step away, the predicate returns `found=true`
    and the witness word reproduces `y` under `WeylWord.kgbCrossLeft`.

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_cross_W_orbit_is_in_cross_orbit_smoke.sml`
*)

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val x = 0
val s = 1
val y = AtlasFFI.atlas_kgb_cross (g, s, x)
val () = if y < 0 then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val pred = CrossWOrbit.is_in_cross_orbit (g, x)
val (found, w) = pred y
val () = if found then () else raise Fail "expected y to be in cross orbit"
val y2 = WeylWord.kgbCrossLeft (g, w, x)
val () = if y2 = y then () else raise Fail "witness does not reproduce y"

val (foundX, wX) = pred x
val () = if foundX andalso null wX then () else raise Fail "expected identity witness at x"

val () = AtlasFFI.atlas_group_free g
val () = TextIO.print "OK\n"

