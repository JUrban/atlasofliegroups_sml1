(* Functional smoke test: build W(E8) character table and check orthogonality. *)

use "atlas-scripts-sml/character_table_E8.sml";
use "atlas-scripts-sml/character_tables.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun main () =
  let
    val ct = CharacterTable_E8.character_table_E8_gap ()
    val () = expect ("|W(E8)|", CharacterTables.order_W ct, 696729600)
    val () = CharacterTables.assert_orthogonality ct
  in
    print "test_character_table_E8_functional_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

