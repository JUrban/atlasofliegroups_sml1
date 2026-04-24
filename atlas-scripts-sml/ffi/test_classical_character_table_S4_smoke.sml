(* Smoke test for `classical_character_tables.sml` (S4 table). *)

use "atlas-scripts-sml/classical_character_tables.sml";
use "atlas-scripts-sml/character_tables.sml";

fun expect (name, cond) = if cond then () else raise Fail name

fun main () =
  let
    val ct = ClassicalCharacterTables.character_table_S 4
    val () = expect ("S4 orthogonal", CharacterTables.check_orthogonality ct)
    val () = expect ("S4 order", CharacterTables.order_W ct = 24)
    val () = expect ("S4 n_irreps", CharacterTables.n_irreps ct = 5)
  in
    print "test_classical_character_table_S4_smoke: ok\n"
  end

val () = main () handle e => (print (exnMessage e ^ "\n"); OS.Process.exit OS.Process.failure);

