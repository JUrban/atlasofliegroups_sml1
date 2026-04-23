use "atlas-scripts-sml/bruhat.sml";

(*
  Smoke test for `Bruhat` (KGB Bruhat order).

  We keep this test light: it checks basic invariants that should hold for any
  real form:
  - reflexivity: x <= x
  - monotonicity wrt length: if x <= y then length(x) <= length(y)
*)

fun assert msg b = if b then () else raise Fail ("assertion failed: " ^ msg)

val g = AtlasFFI.atlas_group_new_simple (#"A", 1, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail ("group_new_simple failed: " ^ AtlasFFI.atlas_last_error ()) else ()

val n = AtlasFFI.atlas_group_kgb_size g
val () = assert "nonempty KGB" (n > 0)

fun len x = AtlasFFI.atlas_kgb_length (g, x)

val sampleN = Int.min (n, 12)
val xs = List.tabulate (sampleN, fn i => i)

val () = List.app (fn x => assert ("reflexive " ^ Int.toString x) (Bruhat.bruhat_leq (g, x, x))) xs

val () =
  List.app
    (fn x =>
       List.app
         (fn y =>
            if Bruhat.bruhat_leq (g, x, y) then
              assert "length monotone" (len x <= len y)
            else
              ())
         xs)
    xs

val () = AtlasFFI.atlas_group_free g
val () = print "OK: bruhat_kgb_smoke\n"

