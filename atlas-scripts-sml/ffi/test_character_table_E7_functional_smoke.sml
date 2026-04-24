(* Functional smoke test: build W(E7) character table and check orthogonality. *)

use "atlas-scripts-sml/character_table_E7.sml";
use "atlas-scripts-sml/character_tables.sml";

fun expect (name, got, want) =
  if got = want then () else raise Fail (name ^ ": expected " ^ Int.toString want ^ ", got " ^ Int.toString got);

fun main () =
  let
    val ct = CharacterTable_E7.character_table_E7_magma ()
    val () = expect ("|W(E7)|", CharacterTables.order_W ct, 2903040)
    val () = CharacterTables.assert_orthogonality ct
  in
    print "test_character_table_E7_functional_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

