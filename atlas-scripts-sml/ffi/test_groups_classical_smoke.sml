(* Smoke test for the classical constructors in `atlas-scripts-sml/groups.sml`. *)
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/groups.sml";

fun assert msg b = if b then () else raise Fail ("assert: " ^ msg)

fun show (name: string, g: AtlasFFI.group) =
  let
    val r = AtlasFFI.atlas_group_rank g
    val ssr = AtlasFFI.atlas_group_semisimple_rank g
    val rfOuter = AtlasFFI.atlas_group_form_number g
    val split = AtlasFFI.atlas_group_is_split g
    val compact = AtlasFFI.atlas_group_is_compact g
    val () =
      print
        ( name ^ ": rank=" ^ Int.toString r ^ " ssr=" ^ Int.toString ssr
          ^ " rfOuter=" ^ Int.toString rfOuter ^ " split=" ^ Int.toString split
          ^ " compact=" ^ Int.toString compact ^ "\n"
        )
  in
    AtlasFFI.atlas_group_free g
  end

val () = show ("SU(2,0) (compact)", Groups.SU (2, 0))
val () = show ("SU(1,1)", Groups.SU (1, 1))
val () = show ("PSU(2,1)", Groups.PSU (2, 1))
val () = show ("SL_R(3)", Groups.SL_R 3)
val () = show ("PSL_R(4)", Groups.PSL_R 4)
val () = show ("Sp_R(4)", Groups.Sp_R 4)
val () = show ("PSp_R(6)", Groups.PSp_R 6)
val () = show ("Sp(2,1)", Groups.Sp (2, 1))
val () = show ("PSp(2,2)", Groups.PSp (2, 2))
val () = show ("Spin(5,3)", Groups.Spin (5, 3))
val () = show ("PSO(6,2)", Groups.PSO (6, 2))

