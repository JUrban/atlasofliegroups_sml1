use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/WeylWord.sml";
use "atlas-scripts-sml/representations.sml";
use "atlas-scripts-sml/Lattice.sml";

(*
  File: atlas-scripts-sml/ffi/test_weyl_word_action_involutive.sml

  Purpose
  - Smoke test for `WeylWord.actRatvec` (FFI Weyl action on rational weights).

  What it checks
  - Acting by `w` and then by `inverse(w)` returns the original weight.

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_weyl_word_action_involutive.sml`
*)

fun eqRatvec (u: Lattice.ratvec, v: Lattice.ratvec) : bool =
  let
    val u = Lattice.ratvecNormalize u
    val v = Lattice.ratvecNormalize v
  in
    #den u = #den v andalso #nums u = #nums v
  end

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val v = Representations.rho g
val w = [0, 1, 2, 3, 2, 1]
val v1 = WeylWord.actRatvec (g, w, v)
val v2 = WeylWord.actRatvec (g, WeylWord.inverse w, v1)

val () = if eqRatvec (v, v2) then () else raise Fail "weyl action inverse check failed"

val () = AtlasFFI.atlas_group_free g
val () = TextIO.print "OK\n"

