(* Functional smoke test: build W(F4) character table and check orthogonality. *)

use "atlas-scripts-sml/character_table_F.sml";
use "atlas-scripts-sml/character_tables.sml";
use "atlas-scripts-sml/RootDatum.sml";

fun main () =
  let
    val rd = RootDatum.newSimple (#"F", 4, false)
    val ct = CharacterTable_F.character_table_F4 rd
    val () = RootDatum.free rd
    val () = CharacterTables.assert_orthogonality ct
  in
    print "test_character_table_F4_functional_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

