use "atlas-scripts-sml/ffi/AtlasFFI.sml";

fun headTokens s n =
  let
    val toks = String.tokens Char.isSpace s
    val first = List.take (toks, Int.min (n, length toks)) handle _ => toks
  in
    String.concatWith " " first
  end

val g2 = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0);
val f4 = AtlasFFI.atlas_group_new_simple (#"F", 4, #"s", 0);

val () = print ("G2_s simple_coroots: " ^ headTokens (AtlasFFI.atlas_group_simple_coroots_text g2) 20 ^ "\n");
val () = print ("G2_s posroots(head): " ^ headTokens (AtlasFFI.atlas_group_posroots_text g2) 20 ^ "\n");

val xOpenF4 = AtlasFFI.atlas_group_kgb_size f4 - 1;
val () =
  print
    ("F4_s involution(x_open) head: "
     ^ headTokens (AtlasFFI.atlas_group_kgb_involution_matrix_text (f4, xOpenF4)) 20
     ^ "\n");

val () = AtlasFFI.atlas_group_free g2;
val () = AtlasFFI.atlas_group_free f4;

