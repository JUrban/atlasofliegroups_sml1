(* Smoke test for `atlas-scripts-sml/groups.sml`. *)
use "atlas-scripts-sml/ffi/AtlasFFI.sml";
use "atlas-scripts-sml/groups.sml";

fun show (name: string, g: AtlasFFI.group) =
  let
    val kgb = AtlasFFI.atlas_group_kgb_size g
    val r = AtlasFFI.atlas_group_rank g
    val ssr = AtlasFFI.atlas_group_semisimple_rank g
    val () =
      print
        ( name ^ ": rank=" ^ Int.toString r ^ ", ssr=" ^ Int.toString ssr ^ ", KGB="
          ^ Int.toString kgb ^ "\n"
        )
  in
    AtlasFFI.atlas_group_free g
  end;

val () = show ("G2_c", Groups.G2_c ());
val () = show ("G2_s", Groups.G2_s ());
val () = show ("F4_s", Groups.F4_s ());
val () = show ("F4_B4", Groups.F4_B4 ());
val () = show ("F4_c", Groups.F4_c ());
val () = show ("E6_c", Groups.E6_c ());
val () = show ("E6_q", Groups.E6_q ());
val () = show ("E6_s", Groups.E6_s ());
val () = show ("E7_s", Groups.E7_s ());
val () = show ("E8_s", Groups.E8_s ());
