(* Smoke test for `WeylWord.kgbCrossRight`. *)

use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/WeylWord.sml";

fun main () =
  let
    val g = AtlasFFI.atlas_group_new_simple (#"A", 2, #"s", 0)
    val () =
      if g = Foreign.Memory.null then
        raise Fail ("test_kgb_cross_right_smoke: failed to create group: " ^ AtlasFFI.atlas_last_error ())
      else
        ()

    val x0 = 0
    val w = [0, 1, 0, 1]

    (* Left cross uses reverse order; right cross uses forward order. *)
    val left = WeylWord.kgbCrossLeft (g, w, x0)
    val right_rev = WeylWord.kgbCrossRight (g, x0, List.rev w)

    val () =
      if left = right_rev then
        ()
      else
        raise Fail "kgbCrossLeft(w,x) <> kgbCrossRight(x,rev w)"

    val () = AtlasFFI.atlas_group_free g
  in
    print "test_kgb_cross_right_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);
