use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/Weylgroup.sml";
use "atlas-scripts-sml/WeylWord.sml";

(*
  File: atlas-scripts-sml/ffi/test_Weylgroup_from_no_Cminus_kgb_smoke.sml

  Purpose
  - Smoke test for `Weylgroup.from_no_Cminus_kgb`.

  What it checks
  - The returned `x0` has no complex descents (`status(s,x0) <> 0` for all s).
  - The witness word `w` satisfies `x = cross(w, x0)` in the `.at` left-cross sense.

  Usage
  - `poly -q < atlas-scripts-sml/ffi/test_Weylgroup_from_no_Cminus_kgb_smoke.sml`
*)

fun checkNoCminus (g: AtlasFFI.group, x: int) : unit =
  let
    val r = AtlasFFI.atlas_group_semisimple_rank g
    fun loop s =
      if s = r then
        ()
      else
        let
          val st = AtlasFFI.atlas_kgb_status (g, s, x)
          val () = if st < 0 then raise Fail (AtlasFFI.atlas_last_error ()) else ()
        in
          if st = 0 then raise Fail ("still has C- at s=" ^ Int.toString s) else loop (s + 1)
        end
  in
    loop 0
  end

val g = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0)
val () = if g = Foreign.Memory.null then raise Fail (AtlasFFI.atlas_last_error ()) else ()

val x = 100
val (w, x0) = Weylgroup.from_no_Cminus_kgb (g, x)
val () = checkNoCminus (g, x0)

val x2 = WeylWord.kgbCrossLeft (g, w, x0)
val () = if x2 = x then () else raise Fail "witness does not reconstruct original x"

val () = AtlasFFI.atlas_group_free g
val () = TextIO.print "OK\n"

