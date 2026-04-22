use "atlas-scripts-sml/ffi/AtlasFFI.sml";

fun show (name, g) =
  let
    val r = AtlasFFI.atlas_group_rank g
    val rho = AtlasFFI.atlas_group_rho_text g
  in
    print (name ^ ": rank=" ^ Int.toString r ^ " rho=" ^ rho ^ "\n")
  end

val f4 = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);
val g2 = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0);

val () = show ("F4_s", f4);
val () = show ("G2_s", g2);

val () = AtlasFFI.atlas_group_free f4;
val () = AtlasFFI.atlas_group_free g2;

