(* Functional smoke test: build W(G2) character table and check orthogonality. *)

use "atlas-scripts-sml/character_table_G.sml";
use "atlas-scripts-sml/character_tables.sml";
use "atlas-scripts-sml/ffi/AtlasFFI.sml";

fun main () =
  let
    val g = AtlasFFI.atlas_group_new_simple (#"G", 2, #"s", 0)
    val ct = CharacterTable_G.character_table_G2 g
    val () = AtlasFFI.atlas_group_free g
    val () = CharacterTables.assert_orthogonality ct
  in
    print "test_character_table_G2_functional_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

